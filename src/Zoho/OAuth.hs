module Zoho.OAuth where

import Network.OAuth.OAuth2 as O
import URI.ByteString as U
import Data.ByteString as BS
import Data.List as DL
import Data.Coerce (coerce)
import Data.Text as T
import Data.String.Conv

mkEndpoint :: Host -> ByteString -> URI
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

newtype Scope = Scope ByteString
newtype ClientId = ClientId Text
newtype ClientSecret = ClientSecret Text


authorizationUrl :: [Scope] -> OAuth2 -> URI
authorizationUrl scopes oa =
  let s = BS.intercalate "," $ DL.map coerce scopes
  in O.appendQueryParams [("scope", s), ("access_type", "offline")] $
     O.authorizationUrl oa
