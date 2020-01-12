{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Zoho.OAuth where

import Network.OAuth.OAuth2 as O hiding (refreshAccessToken)
import Network.OAuth.OAuth2.TokenRequest as TokenRequest
import URI.ByteString as U
import URI.ByteString.QQ as U
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.List as DL
import Data.Coerce (coerce)
import Data.Text as T
import Data.String.Conv
import Network.HTTP.Client as HC -- (Manager(..), newManager, Request, ManagerSettings(..), requestBody, requestHeaders, RequestBody(..), Response(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types as HT
import Network.Wreq as W
import Control.Lens
import Data.Aeson (FromJSON)
-- import Network.Wreq.Types (Postable)
-- import Zoho.Types (zohoResponseChecker)
import Data.Aeson as Aeson
import Data.String.Conv
import Zoho.Types (OrgId(..))

mkEndpoint :: Host -> BS.ByteString -> URI
mkEndpoint h p = URI
  { uriScheme = Scheme "https"
  , uriAuthority = Just $ Authority
                   { authorityUserInfo = Nothing
                   , authorityHost = h
                   , authorityPort = Nothing
                   }
  , uriPath = p
  , uriQuery = Query []
  , uriFragment = Nothing
  }

mkAuthEndpoint :: Host -> URI
mkAuthEndpoint h = mkEndpoint h "/oauth/v2/auth"

mkTokenEndpoint :: Host -> URI
mkTokenEndpoint h = mkEndpoint h "/oauth/v2/token"

mkApiEndpoint :: BS.ByteString -> URI
mkApiEndpoint p = mkEndpoint (Host "www.zohoapis.com") p

hostUS :: Host
hostUS = Host "accounts.zoho.com"

hostAU :: Host
hostAU = Host "accounts.zoho.com.au"

hostEU :: Host
hostEU = Host "accounts.zoho.eu"

hostIN :: Host
hostIN = Host "accounts.zoho.in"

hostCN :: Host
hostCN = Host "accounts.zoho.com.cn"

-- authEndpointUS :: URI
-- authEndpointUS = mkAuthEndpoint hostUS

-- authEndpointAU :: URI
-- authEndpointAU = mkAuthEndpoint hostAU

-- authEndpointEU :: URI
-- authEndpointEU = mkAuthEndpoint hostEU

-- authEndpointIN :: URI
-- authEndpointIN = mkAuthEndpoint hostIN

-- authEndpointCN :: URI
-- authEndpointCN = mkAuthEndpoint hostCN

-- tokenEndpointUS :: URI
-- tokenEndpointUS = mkTokenEndpoint hostUS

-- tokenEndpointAU :: URI
-- tokenEndpointAU = mkTokenEndpoint hostAU

-- tokenEndpointEU :: URI
-- tokenEndpointEU = mkTokenEndpoint hostEU

-- tokenEndpointIN :: URI
-- tokenEndpointIN = mkTokenEndpoint hostIN

-- tokenEndpointCN :: URI
-- tokenEndpointCN = mkTokenEndpoint hostCN

mkOAuth :: Host -> ClientId -> ClientSecret -> URI -> O.OAuth2
mkOAuth h cid sec cback = O.OAuth2
  { oauthClientId = coerce cid
  , oauthClientSecret = coerce sec
  , oauthOAuthorizeEndpoint = mkAuthEndpoint h
  , oauthAccessTokenEndpoint = mkTokenEndpoint h
  , oauthCallback = Just cback
  }

newtype Scope = Scope BS.ByteString
newtype ClientId = ClientId Text
newtype ClientSecret = ClientSecret Text


authorizationUrl :: [Scope] -> OAuth2 -> URI
authorizationUrl scopes oa =
  let s = BS.intercalate "," $ DL.map coerce scopes
  in O.appendQueryParams [("scope", s), ("access_type", "offline")] $
     O.authorizationUrl oa

-- addAuthHeader :: Manager -> AccessToken -> Options -> Options
-- addAuthHeader mgr (AccessToken tkn) opt = opt
--   & (header "Authorization") .~ ["Zoho-oauthtoken " <> toS tkn]
--   & W.manager .~ (Right mgr)
--   & W.checkResponse .~ (Just zohoResponseChecker)


-- authGetJSON :: Options
--             -> String
--             -> Manager
--             -> AccessToken
--             -> IO (Response BSL.ByteString)
-- authGetJSON opt uri mgr tkn = W.getWith (addAuthHeader mgr tkn opt) uri

-- authPost :: (Postable a)
--          => Options
--          -> String
--          -> a
--          -> Manager
--          -> AccessToken
--          -> IO (Response BSL.ByteString)
-- authPost opt uri pload mgr tkn = W.postWith (addAuthHeader mgr tkn opt) uri pload

uriAppendPathFragment :: BS.ByteString
                      -> URI
                      -> URI
uriAppendPathFragment p u = u{uriPath=(uriPath u) <> p}

setRequestHeaders :: RequestHeaders
                  -> Request
                  -> Request
setRequestHeaders h req = req{requestHeaders=h}

addRequestHeaders :: RequestHeaders
                  -> Request
                  -> Request
addRequestHeaders h req = req{requestHeaders=h ++ (requestHeaders req)}

removeRequestHeader :: HeaderName
                    -> Request
                    -> Request
removeRequestHeader n req =
  let h = DL.deleteBy (\x y -> fst x == fst y) (n, "") (requestHeaders req)
  in req{requestHeaders=h}

replaceRequestHeader :: Header
                     -> Request
                     -> Request
replaceRequestHeader (hname, hval) req =
  addRequestHeaders [(hname, hval)] $
  removeRequestHeader hname req

replaceAuthHeader :: AccessToken
                  -> Request
                  -> Request
replaceAuthHeader (AccessToken tkn) req =
  replaceRequestHeader (hAuthorization, "Zoho-oauthtoken " <> toS tkn) req

addOrgIdHeader :: OrgId
               -> Request
               -> Request
addOrgIdHeader (OrgId oid) req =
  addRequestHeaders [("orgId", toS oid)] req

prepareGet :: URI
           -> [(BS.ByteString, Maybe BS.ByteString)]
           -> RequestHeaders
           -> Request
prepareGet u q h =
  setRequestHeaders h $
  setQueryString q $
  parseRequest_ $ "GET " <> (toS $ serializeURIRef' u)

preparePost :: URI
            -> [(BS.ByteString, Maybe BS.ByteString)]
            -> RequestHeaders
            -> BSL.ByteString
            -> Request
preparePost = prepareWithPayload "POST"

prepareFormPost :: (HT.QueryLike formdata)
                => URI
                -> HT.Query
                -> RequestHeaders
                -> formdata
                -> Request
prepareFormPost u q h formdata =
  let req = replaceRequestHeader (hContentType, "application/x-www-form-urlencoded") $
            prepareWithoutPayload "POST" u q h
  in req{requestBody = RequestBodyBS (HT.renderQuery False $ toQuery formdata)}

prepareJSONPost :: (ToJSON a)
                => URI
                -> [(BS.ByteString, Maybe BS.ByteString)]
                -> RequestHeaders
                -> a
                -> Request
prepareJSONPost u q h pload =
  prepareWithPayload "POST" u q h (Aeson.encode pload)

prepareDelete :: URI
              -> [(BS.ByteString, Maybe BS.ByteString)]
              -> RequestHeaders
              -> Maybe BSL.ByteString
              -> Request
prepareDelete u q h mPload =
  case mPload of
    Nothing -> prepareWithoutPayload "DELETE" u q h
    Just pload -> prepareWithPayload "DELETE" u q h pload

prepareWithoutPayload :: String
                      -> URI
                      -> [(BS.ByteString, Maybe BS.ByteString)]
                      -> RequestHeaders
                      -> Request
prepareWithoutPayload method u q h =
  setRequestHeaders h $
  setQueryString q $
  parseRequest_ $ method <> " " <> (toS $ serializeURIRef' u)

prepareWithPayload :: String
                   -> URI
                   -> [(BS.ByteString, Maybe BS.ByteString)]
                   -> RequestHeaders
                   -> BSL.ByteString
                   -> Request
prepareWithPayload method u q h pload =
  let req = prepareWithoutPayload method u q h
  in req{requestBody=RequestBodyLBS pload}


runRequest :: Request
           -> Manager
           -> AccessToken
           -> IO (Response BSL.ByteString)
runRequest req mgr tkn = httpLbs (replaceAuthHeader tkn req) mgr

testToken :: RefreshToken
testToken = RefreshToken "1000.d172fccaf6d7e1e08ec40af3cbf05af6.fa961eedf1fa4b2cfbe822439b376bb0"


handleOAuth2TokenResponse :: FromJSON err
                          => Response BSL.ByteString
                          -> OAuth2Result err BSL.ByteString
handleOAuth2TokenResponse rsp =
  if HT.statusIsSuccessful (HC.responseStatus rsp)
  then Right $ HC.responseBody rsp
  else Left $ parseOAuth2Error (HC.responseBody rsp)

refreshAccessToken :: Manager
                   -> OAuth2
                   -> RefreshToken
                   -> IO (OAuth2Result TokenRequest.Errors OAuth2Token)
refreshAccessToken mgr OAuth2{oauthClientId, oauthClientSecret, oauthAccessTokenEndpoint} (RefreshToken rtkn) =
  fmap (parseResponseFlexible . handleOAuth2TokenResponse) $
  W.postWith
    (W.defaults & W.manager .~ (Right mgr))
    (toS $ serializeURIRef' $ oauthAccessTokenEndpoint)
    [ "refresh_token" W.:= rtkn
    , "client_id" W.:= oauthClientId
    , "client_secret" W.:= oauthClientSecret
    , "grant_type" W.:= ("refresh_token" :: Text)
    ]

-- TODO: Retry on network errors
withAccessToken :: Manager
                -> OAuth2
                -> RefreshToken
                -> Maybe AccessToken
                -> (Manager -> AccessToken -> IO a)
                -> IO (Either String (a, (AccessToken, Maybe RefreshToken)))
withAccessToken mgr oa rtkn mAtkn fn = case mAtkn of
  Just x -> do
    r <- fn mgr x
    pure $ Right (r, (x, Nothing))
  Nothing -> (refreshAccessToken mgr oa rtkn) >>= \case
    Left e -> pure $ Left $ show e
    Right OAuth2Token{accessToken, refreshToken} -> do
      r <- fn mgr accessToken
      pure $ Right (r, (accessToken, refreshToken))

logRequest :: Request -> IO Request
logRequest r = do
  Prelude.putStrLn $ show r
  case requestBody r of
    RequestBodyLBS x -> BSL.putStrLn x
    RequestBodyBS x -> BS.putStrLn x
    _ -> Prelude.putStrLn "Cannot print body"
  Prelude.putStrLn $ show $ requestHeaders r
  pure r

applyOptionalQueryParam :: (StringConv a BS.ByteString)
                        => BS.ByteString
                        -> Maybe a
                        -> HT.Query
                        -> HT.Query
applyOptionalQueryParam k mVal qp = case mVal of
  Nothing -> qp
  Just val -> (k, Just $ toS val):qp
