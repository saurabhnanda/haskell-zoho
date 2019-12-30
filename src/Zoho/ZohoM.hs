module Zoho.ZohoM where

import Control.Monad.IO.Class
import Control.Monad
import Network.OAuth.OAuth2 as O hiding (refreshAccessToken)
import Network.OAuth.OAuth2.TokenRequest as TokenRequest
import Data.ByteString.Lazy as BSL
import Network.Wreq as W hiding (manager)
import qualified Network.Wreq as W
import Network.HTTP.Client as HC (Manager(..), newManager, Request, ManagerSettings(..), requestBody, requestHeaders, RequestBody(..), Response(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.IORef
import Control.Lens
import Control.Monad.Reader
import Network.Wreq.Types (Postable)
import URI.ByteString as U
import Data.Text (Text)
import Data.String.Conv (toS)
import Data.Aeson as Aeson hiding (Options)
import Network.HTTP.Types as HT
import Data.Maybe


class (MonadIO m) => HasZoho m where
  refreshAccessToken :: m (OAuth2Result TokenRequest.Errors OAuth2Token)

  authGet :: Options -> String -> m (Response BSL.ByteString)
  authPost :: (Postable a) => Options -> String -> a -> m (Response BSL.ByteString)

  getManager :: m Manager
  getRefreshToken :: m RefreshToken
  setRefreshToken :: RefreshToken -> m ()
  getAccessToken :: m AccessToken
  setAccessToken :: AccessToken -> m ()
  getOAuth2Credentials :: m OAuth2

data ZohoEnv = ZohoEnv
  { zenvRefreshTokenRef :: IORef RefreshToken
  , zenvAccessTokenRef :: IORef AccessToken
  , zenvManager :: Manager
  , zenvOAuth2 :: OAuth2
  }
$(makeLensesWith abbreviatedFields ''ZohoEnv)

oauth2 :: (HasOAuth2 s a) => Lens' s a
oauth2 = oAuth2

type ZohoT = ReaderT ZohoEnv

runZohoT :: (MonadIO m) => Manager -> OAuth2 -> RefreshToken -> Maybe AccessToken -> ZohoT m a -> m a
runZohoT mgr oa rtkn mAtkn action = do
  rtknRef <- liftIO $ newIORef rtkn
  atknRef <- liftIO $ newIORef $ fromMaybe (AccessToken "(none)") mAtkn
  let zenv = ZohoEnv
        { zenvRefreshTokenRef = rtknRef
        , zenvAccessTokenRef = atknRef
        , zenvManager = mgr
        , zenvOAuth2 = oa
        }
  runReaderT action zenv

instance (MonadIO m) => HasZoho (ZohoT m) where
  getManager = view manager
  getOAuth2Credentials = view oauth2
  getRefreshToken = (view refreshTokenRef) >>= (liftIO . readIORef)
  setRefreshToken tkn = do
    x <- view refreshTokenRef
    liftIO $ atomicModifyIORef' x (\_ -> (tkn, ()))

  getAccessToken = (view accessTokenRef) >>= (liftIO . readIORef)
  setAccessToken tkn = do
    x <- view accessTokenRef
    liftIO $ atomicModifyIORef' x (\_ -> (tkn, ()))

  refreshAccessToken = defaultRefreshAccessToken


handleOAuth2TokenResponse :: FromJSON err
                          => Response BSL.ByteString
                          -> OAuth2Result err BSL.ByteString
handleOAuth2TokenResponse rsp =
  if HT.statusIsSuccessful (HC.responseStatus rsp)
  then Right $ HC.responseBody rsp
  else Left $ parseOAuth2Error (HC.responseBody rsp)

defaultRefreshAccessToken :: (HasZoho m)
                          => m (OAuth2Result TokenRequest.Errors OAuth2Token)
defaultRefreshAccessToken = do
  mgr <- getManager
  OAuth2{oauthClientId, oauthClientSecret, oauthAccessTokenEndpoint } <- getOAuth2Credentials
  (RefreshToken rtkn) <- getRefreshToken
  r <- liftIO $ W.postWith
       (W.defaults & W.manager .~ (Right mgr))
       (toS $ serializeURIRef' $ oauthAccessTokenEndpoint)
       [ "refresh_token" W.:= rtkn
       , "client_id" W.:= oauthClientId
       , "client_secret" W.:= oauthClientSecret
       , "grant_type" W.:= ("refresh_token" :: Text)
       ]
  case (O.parseResponseFlexible $ handleOAuth2TokenResponse r) of
    Left e -> pure $ Left e
    Right oa@OAuth2Token{accessToken, refreshToken} -> do
      setAccessToken accessToken
      maybe (pure ()) setRefreshToken refreshToken
      pure $ Right oa

