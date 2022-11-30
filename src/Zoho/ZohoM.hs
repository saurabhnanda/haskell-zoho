{-# LANGUAGE TupleSections #-}
module Zoho.ZohoM where

import Control.Monad.IO.Class
import Control.Monad
import Network.OAuth.OAuth2 as O hiding (refreshAccessToken)
import Network.OAuth.OAuth2.TokenRequest as TokenRequest
import Data.ByteString.Lazy as BSL
import Network.Wreq as W hiding (manager)
import qualified Network.Wreq as W
import Network.HTTP.Client as HC (Manager(..), newManager, Request, ManagerSettings(..), requestBody, requestHeaders, RequestBody(..), Response(..), method, HttpException(..), HttpExceptionContent(..), brConsume, redirectCount, httpLbs)
import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Data.IORef
import Control.Lens
import Control.Monad.Reader
import Network.Wreq.Types (Postable)
import URI.ByteString as U
import Data.Text (Text)
import Data.String.Conv (toS)
import Data.Aeson as Aeson hiding (Options)
import Data.Aeson.Lens
import Network.HTTP.Types as HT
import Data.Maybe
import Zoho.Types
import qualified Control.Monad.Catch as E
import Data.Either
import Control.Retry as Retry
import Network.Wreq.Types as W (ResponseChecker)
import Debug.Trace
import Data.Functor (void)
import Data.Coerce (coerce)
import qualified Zoho.OAuth as ZO
import Data.List.NonEmpty as NE
import Zoho.CRM.Common (ZohoResult(..))
import Data.ByteString as BS
import Data.List as DL
import UnliftIO.MVar
import UnliftIO.IORef
import Control.Monad.IO.Unlift

class (MonadIO m, E.MonadMask m) => HasZoho m where
  refreshAccessToken :: AccessToken -> m (OAuth2Result TokenRequest.Errors (RefreshToken, AccessToken))
  getManager :: m Manager
  getRefreshToken :: m RefreshToken
  getAccessToken :: m AccessToken
  -- setTokens :: (Maybe RefreshToken, Maybe AccessToken) -> m (RefreshToken, AccessToken)
  getOAuth2Credentials :: m OAuth2
  runRequest :: Request -> m (Response BSL.ByteString)

data ZohoEnv = ZohoEnv
  { zenvTokenRef :: MVar (RefreshToken, AccessToken)
  , zenvManager :: Manager
  , zenvOAuth2 :: OAuth2
  -- , zenvRefreshTokenLock :: MVar ()
  }
$(makeLensesWith abbreviatedFields ''ZohoEnv)

oauth2 :: (HasOAuth2 s a) => Lens' s a
oauth2 = oAuth2

type ZohoT = ReaderT ZohoEnv

runZohoT :: (MonadIO m)
         => Manager
         -> OAuth2
         -> RefreshToken
         -> Maybe AccessToken
         -> ZohoT m a
         -> m a
runZohoT mgr oa rtkn mAtkn action = do
  tknRef <- newMVar (rtkn, fromMaybe (AccessToken "(none)") mAtkn)
  let zenv = ZohoEnv
        { zenvTokenRef = tknRef
        , zenvManager = mgr
        , zenvOAuth2 = oa
        }
  runReaderT action zenv

instance (MonadIO m, E.MonadMask m, MonadUnliftIO m) => HasZoho (ZohoT m) where
  runRequest req = defaultRunRequest True req
  getManager = view manager
  getOAuth2Credentials = view oauth2
  getRefreshToken = do
    ref <- view tokenRef
    withMVar ref (pure . fst)
  getAccessToken = do
    ref <- view tokenRef
    withMVar ref (pure . snd)
  -- setTokens (mrtkn, matkn) = do
  --   tknRef <- view tokenRef
  --   atomicModifyIORef' tknRef $ \(exrtkn, exatkn) ->
  --     let x = ( fromMaybe exrtkn mrtkn
  --             , fromMaybe exatkn matkn )
  --     in (x, x)

  refreshAccessToken failingAtkn = do
    traceM "\n\n =======> REFRESH TOKEN ATTEMPT ========= \n\n"
    ref <- view tokenRef
    modifyMVar ref $ \curTkns@(rtkn, atkn) -> do
      if (coerce failingAtkn :: Text) /= (coerce atkn)
        -- This thread was trying to refresh the token, but it has already been
        -- refreshed by some other thread
        then pure $ traceShowId (curTkns, Right curTkns)

        -- This seems to tbe the first thread that is hitting this code-path. So
        -- it is responsibile for actually performing the token-refres
        else (defaultRefreshAccessToken rtkn) >>= \case
          Left e ->
            pure (curTkns, Left e)
          Right oa@OAuth2Token{accessToken, refreshToken} ->
            let x = (fromMaybe rtkn refreshToken, accessToken)
            in pure $ traceShow "\n\n =======> TOKEN REFRESHED ========= \n\n" (x, Right x)


handleOAuth2TokenResponse :: FromJSON err
                          => Response BSL.ByteString
                          -> OAuth2Result err BSL.ByteString
handleOAuth2TokenResponse rsp =
  if HT.statusIsSuccessful (HC.responseStatus rsp)
  then Right $ HC.responseBody rsp
  else Left $ parseOAuth2Error (HC.responseBody rsp)

defaultRefreshAccessToken :: (HasZoho m)
                          => RefreshToken
                          -> m (OAuth2Result TokenRequest.Errors OAuth2Token)
defaultRefreshAccessToken (RefreshToken rtkn) = do
  traceM "\n\n =======> ACTUAL REFRESH TOKEN ========= \n\n"

  mgr <- getManager
  OAuth2{oauthClientId, oauthClientSecret, oauthAccessTokenEndpoint } <- getOAuth2Credentials
  r <- defaultRunRequest False $
       ZO.prepareFormPost oauthAccessTokenEndpoint [] []
       [ ("refresh_token" :: Text, rtkn)
       , ("client_id", oauthClientId)
       , ("client_secret", fromMaybe (Prelude.error "client_secret is required") oauthClientSecret)
       , ("grant_type", "refresh_token")
       ]

  traceM $ "\n\n =======> TOKEN REFRESH RESPONSE: " <> show r
  pure $ ZO.parseResponseFlexible $ handleOAuth2TokenResponse r

-- addAuthHeader :: (HasZoho m)
--               => Options
--               -> m Options
-- addAuthHeader opt = do
--   mgr <- getManager
--   (AccessToken tkn) <- getAccessToken
--   pure $ opt
--     & (header "Authorization") .~ ["Zoho-oauthtoken " <> toS tkn]
--     & W.manager .~ (Right mgr)
--     & W.checkResponse .~ (Just zohoResponseChecker)

-- authGet :: (HasZoho m)
--         => Options
--         -> String
--         -> m (Response BSL.ByteString)
-- authGet opt uri = do
--   h <- addAuthHeader opt
--   liftIO $ W.getWith h uri

-- authPost :: (HasZoho m, Postable a)
--          => Options
--          -> String
--          -> a
--          -> m (Response BSL.ByteString)
-- authPost opt uri pload = do
--   h <- addAuthHeader opt
--   liftIO $ W.postWith h uri pload

zohoMaximumRetries :: Int
zohoMaximumRetries = 5

zohoRetryPolicy :: RetryPolicy
zohoRetryPolicy =
  (Retry.exponentialBackoff 10000) <> (Retry.limitRetries zohoMaximumRetries)

-- runRequest :: (FromJSON a, HasZoho m, E.MonadMask m)
--            => (Manager -> AccessToken -> IO (W.Response BSL.ByteString))
--            -> m (Either Error a)
-- runRequest action = do
--   mgr <- getManager
--   -- TODO: Handle HttpException and retry on oauth failures
--   r <- Retry.recovering
--        zohoRetryPolicy
--        [const $ E.Handler zohoRetriableHandler]
--        (const $ modifiedAction mgr)

--   case eitherDecode $ r ^. W.responseBody of
--     Left e -> pure $ Left $ ParseError e (r ^. W.responseBody)
--     Right r -> pure $ Right r

--   -- TODO: Try parsing into error object? Does that even depend on HTTP status
--   -- code?
--   where
--     modifiedAction mgr = do
--       atkn <- getAccessToken
--       r <- liftIO $ retryOnTemporaryNetworkErrors $ action mgr atkn
--       case r ^. W.responseStatus . W.statusCode of
--         401 -> case traceShowId $ eitherDecode $ r ^. W.responseBody of
--           Left e -> pure r
--           Right ZohoError{zerrCode} -> case zerrCode of
--             ZErrInvalidToken -> do
--               -- check if AccessToken has been changed by the time we got here.
--               -- If requests are being made concurrently, then it is possible
--               -- that some other invocation of `runRequest` has already performed
--               -- the `refreshAccessToken` step and we don't need to do it again.
--               atkn2 <- getAccessToken
--               if (coerce atkn :: Text) /= (coerce atkn2)
--                 then E.throwM ZohoRetriableException
--                 else do refreshAccessToken >>= \case
--                           Left e -> E.throwM $ TokenError e
--                           Right _ -> E.throwM ZohoRetriableException
--             _ -> pure r
--         _ -> pure r




--     zohoRetriableHandler (e :: ZohoRetriableException) = pure True
    -- httpExceptionHandler (e :: HttpException) =
    --   pure $ case e of
    --     InvalidUrlException _ _ -> False
    --     HttpExceptionRequest req c ->
    --       case c of
    --         -- Never retry. Seems like a logical bug in the requst
    --         -- or a permanent issue with the networking environment
    --         TooManyRedirects _ -> False
    --         OverlongHeaders -> False
    --         InvalidStatusLine _ -> False
    --         InvalidHeader _ -> False
    --         InternalException _ -> False
    --         TlsNotSupported -> False
    --         WrongRequestBodyStreamSize _ _ -> False
    --         InvalidProxyEnvironmentVariable _ _ -> False
    --         InvalidProxySettings _ -> False

    --         -- Always retry. Even if this was a POST/PUT/DELETE request, because
    --         -- all of these errors indicate that the request didn't event reach
    --         -- the remove server.
    --         ConnectionTimeout -> True
    --         ConnectionFailure _ -> True
    --         ProxyConnectException _ _ _ -> True
    --         InvalidDestinationHost _ -> True

    --         -- Retry ONLY in the case of GET requests. Retrying in the case of
    --         -- POST/PUT/DELETE might result in the action being performed again,
    --         -- resulting in unexpected results.
    --         ResponseTimeout -> isGet req
    --         NoResponseDataReceived -> isGet req
    --         ResponseBodyTooShort _ _ -> isGet req
    --         InvalidChunkHeaders -> isGet req
    --         IncompleteHeaders -> isGet req
    --         HttpZlibException _ -> isGet req
    --         ConnectionClosed -> isGet req
    --         x@(StatusCodeException _ _) -> isGet req
    --           -- error $ "Not expecting a StatusCodeException to be raised after using Wreq.checkResponse\n" <>
    --           -- show x <>
    --           -- "\n-----------"

retryOnTemporaryNetworkErrors :: (MonadIO m, E.MonadMask m) => m a -> m a
retryOnTemporaryNetworkErrors action = Retry.recovering
  zohoRetryPolicy
  [const $ E.Handler httpExceptionHandler]
  (const action)
  where
    httpExceptionHandler (e :: HttpException) =
      pure $ case e of
      InvalidUrlException _ _ -> False
      HttpExceptionRequest req c ->
        case c of
          -- Never retry. Seems like a logical bug in the requst
          -- or a permanent issue with the networking environment
          TooManyRedirects _ -> False
          OverlongHeaders -> False
          InvalidStatusLine _ -> False
          InvalidHeader _ -> False
          InternalException _ -> False
          TlsNotSupported -> False
          WrongRequestBodyStreamSize _ _ -> False
          InvalidProxyEnvironmentVariable _ _ -> False
          InvalidProxySettings _ -> False

          -- Always retry. Even if this was a POST/PUT/DELETE request, because
          -- all of these errors indicate that the request didn't event reach
          -- the remove server.
          ConnectionTimeout -> True
          ConnectionFailure _ -> True
          ProxyConnectException _ _ _ -> True
          InvalidDestinationHost _ -> True

          -- Retry ONLY in the case of GET requests. Retrying in the case of
          -- POST/PUT/DELETE might result in the action being performed again,
          -- resulting in unexpected results.
          ResponseTimeout -> isGet req
          NoResponseDataReceived -> isGet req
          ResponseBodyTooShort _ _ -> isGet req
          InvalidChunkHeaders -> isGet req
          IncompleteHeaders -> isGet req
          HttpZlibException _ -> isGet req
          ConnectionClosed -> isGet req
          StatusCodeException resp _ ->
            case (resp ^. W.responseStatus . W.statusCode) of
              408 -> True -- request timeout
              409 -> True -- Conflict
              412 -> True -- precondition failed
              417 -> True -- Expectation failed
              420 -> True -- Enhance your calm
              429 -> True -- Too many requests
              _ -> False

            -- error $ "Not expecting a StatusCodeException to be raised after using Wreq.checkResponse\n" <>
            -- show x <>
            -- "\n-----------"
isRetryableStatusCode :: Int -> Bool
isRetryableStatusCode s = case s of
  408 -> True -- request timeout
  409 -> True -- Conflict
  412 -> True -- precondition failed
  417 -> True -- Expectation failed
  420 -> True -- Enhance your calm
  429 -> True -- Too many requests
  _ -> False

isGet :: Request -> Bool
isGet req = HT.methodGet == (HC.method req)

-- zohoWreqDefaults :: W.Options
-- zohoWreqDefaults = W.defaults & W.checkResponse .~ (Just zohoResponseChecker)

parseResponse :: (FromJSON a)
              => BSL.ByteString
              -> Either Error a
parseResponse rbody =
  case (eitherDecode rbody) of
    Left e -> Left $ ParseError e rbody
    Right r -> Right r

runRequestAndParseOptionalResponse :: (HasZoho m, FromJSON a)
                                   => b
                                   -> (a -> b)
                                   -> Request
                                   -> m (Either Error b)
runRequestAndParseOptionalResponse emptyVal transformFn req = do
  res <- runRequest req
  let rbody = HC.responseBody res
      stcode = HT.statusCode $ HC.responseStatus res
  if rbody == mempty || stcode==204
    then pure $ Right emptyVal
    else case parseResponse rbody of
           Left e -> pure $ Left e
           Right a ->
             pure $ Right $ transformFn a

runRequestAndParseResponse :: forall m a . (HasZoho m, E.MonadMask m, FromJSON a)
                           => Request
                           -> m (Either Error a)
runRequestAndParseResponse req = do
  res <- runRequest req
  pure $ parseResponse $ HC.responseBody res


defaultRunRequest :: (HasZoho m, E.MonadMask m)
                  => Bool
                  -> Request
                  -> m (Response BSL.ByteString)
defaultRunRequest isAuthenticated req = do
  mgr <- getManager
  Retry.recovering
    zohoRetryPolicy
    [const $ E.Handler zohoRetriableHandler]
    (modifiedAction mgr)
  where
    zohoRetriableHandler (e :: ZohoRetriableException) = pure True
    throwHttpException res =
      E.throwM $ HttpExceptionRequest req $ StatusCodeException (void res) (toS $ HC.responseBody res)

    handleSecurityError mAtkn r =
      case mAtkn of
        Nothing -> throwHttpException r
        Just atkn -> (refreshAccessToken atkn) >>= \case
          Left e -> E.throwM $ TokenError e
          Right _ -> E.throwM ZohoRetriableException

    modifiedAction mgr RetryStatus{rsIterNumber} = do
      (mAtkn, r) <- case isAuthenticated of
        False ->
          (Nothing, ) <$> (liftIO $ retryOnTemporaryNetworkErrors $ httpLbs req mgr)
        True -> do
          let finalReq = req{redirectCount=0}
          atkn <- getAccessToken
          (Just atkn,) <$> (liftIO $ retryOnTemporaryNetworkErrors $ ZO.runRequest finalReq mgr atkn)

      traceM  $ "\n\n" <> show r <> "\n\n"
      case (HT.statusCode $ HC.responseStatus r) of
        200 -> pure r
        204 -> pure r
        201 -> pure r
        302 -> case DL.lookup "Location" (HC.responseHeaders r) of
          Nothing -> handleSecurityError mAtkn r
          Just l ->
            if BS.isInfixOf "IAMSecurityError" l
            then handleSecurityError mAtkn r
            else pure r
        401 ->
          case (HC.responseBody r) ^? (key "code") of
            Just (Aeson.String "INVALID_TOKEN") -> handleSecurityError mAtkn r
            Just (Aeson.String "INVALID_OAUTH") -> handleSecurityError mAtkn r
            Just (Aeson.Number 57) -> handleSecurityError mAtkn r
            _ -> throwHttpException r

        st -> if (isRetryableStatusCode st) && (rsIterNumber == (zohoMaximumRetries - 1))
              then E.throwM ZohoRetriableException
              else throwHttpException r

