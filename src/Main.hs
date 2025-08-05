{-# LANGUAGE MultiWayIf #-}

module Main where

import Zoho.Desk.IM as IM
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS
import Network.OAuth.OAuth2 as O hiding (refreshAccessToken)
import Zoho.OAuth as ZO
import Zoho.ZohoM as ZM
import Zoho.Types
import Data.Aeson as Aeson
import URI.ByteString.QQ
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as CL8
import Data.String.Conv (toS)
import Data.List as DL

getLoggingManager :: 
  (HTTP.Request -> IO HTTP.Request) -> 
  (HTTP.Response HTTP.BodyReader -> IO (HTTP.Response HTTP.BodyReader)) -> 
  IO HTTP.Manager
getLoggingManager logReq logRes = newTlsManagerWith s
  where
    s = tlsManagerSettings
      { HTTP.managerModifyRequest = logReq
      , HTTP.managerModifyResponse = logRes
      }

httpRequestToCurl :: HTTP.Request -> BSL.ByteString
httpRequestToCurl req = 
  let isPost :: BSL.ByteString = if (HTTP.method req)=="POST" then "-XPOST" else ""
      proto  :: BSL.ByteString = if (HTTP.secure req) then "https" else "http"
      port_  :: BSL.ByteString = 
        if  | (HTTP.secure req) && (HTTP.port req)==443 -> ""
            | (not $ HTTP.secure req) && (HTTP.port req)==80 -> ""
            | otherwise -> toS $ show $ HTTP.port req
      headers :: BSL.ByteString = 
        CL8.intercalate " " $
          (flip DL.map) (HTTP.requestHeaders req) $ \(k, v) ->
            "--header " <> toS (CaseInsensitive.original k) <> " " <> toS v
      body :: BSL.ByteString
      body =  
        if (HTTP.method req) `elem` ["GET", "DELETE", "HEAD", "OPTIONS"]
        then ""
        else case HTTP.requestBody req of
          HTTP.RequestBodyLBS bs -> "--data '" <> bs <> "'"
          HTTP.RequestBodyBS bs -> "--data '" <> toS bs <> "'"
          _ -> "'complicated body'"
  in "curl " <> isPost <> " " <> headers <> " " <> body <> " " <> proto <> "://" <> toS (HTTP.host req) <> toS port_ <> toS (HTTP.path req) <> toS (HTTP.queryString req)

main :: IO (Either Error Aeson.Value)
main = do
  let oid = OrgId "104397711"
      chId = ChannelId "10416000000006001"
      req = ZohoDeskInitiateIMSessionReq 
            { reqReceiverId = "+917798722177"
            , reqReceiverType = "PHONENUMBER"
            , reqCannedMessageId = "10416000000040043"
            , reqLanguage = "en"
            , reqMessage = "ACTION REQUIRED\nThis is a reminder for an open invoice against your Vacation Labs account. We request you to please check your email for invoice and payment details. \nFailure to pay the invoice by the due-date will result in suspension of your Vacation Labs account (i.e. your website / booking-engine will stop working)."
            , reqHeaderMessage = Nothing
            }
  mgr <- getLoggingManager (\req -> BSL.putStrLn (httpRequestToCurl req) >> pure req) pure
  let oa = ZO.mkOAuth ZO.hostUS (ZO.ClientId "1000.DQV1LY2MRP8OMPXMK9DLYZVPGAF1VH") (ZO.ClientSecret "92bd87d164bf6f671cc6400d20da1a0bb6a1570dde") [uri|http://master.hetzner.vacationlabs.com/lambda/oauth-redirect|]
      rtkn = RefreshToken "1000.2c258f8e575af7ebda4fe88363569ad8.249a505687b32d97443b7cee4a7d0761"
  ZM.runZohoT mgr oa rtkn Nothing $ do
    initiateIMSession oid chId req