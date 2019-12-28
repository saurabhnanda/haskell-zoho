module Test where

import Zoho.Types
import Network.OAuth.OAuth2 as O
import Zoho.OAuth as ZO
import Data.String.Conv
import Zoho.CRM.Records
import URI.ByteString
import URI.ByteString.QQ
import Network.HTTP.Client (Manager, ManagerSettings(..), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.ByteString as BS
import Network.Wreq as W
import Control.Lens
import Zoho.CRM.Contacts as Contacts
import Data.Aeson as Aeson

zohoOAuth :: OAuth2
zohoOAuth = mkOAuth hostUS (ClientId "1000.PCRP10N4ZKXC7F029BTTP6UT594BIH") (ClientSecret "67d211c3cb5c31df1a1899462514fba3abe152f6cb") ([uri|http://master.hetzner.vacationlabs.com/lambda/oauth-redirect|])

zohoManager :: IO Manager
zohoManager = newManager $ tlsManagerSettings{managerModifyRequest=logRequest}

test :: IO (Maybe (Either String (PaginatedResponse "data" [Contact Aeson.Value])))
test = do
  let rtkn = RefreshToken "1000.7950f276ab5889010ba61d5074835d16.84a6e76f73e09303f32e408c5ccb298f"
  mgr <- zohoManager
  x <- withAccessToken mgr zohoOAuth rtkn Nothing (Contacts.list defaultListOptions{optPerPage=(Just 10)})
  pure $ x ^? _Right . _1 . W.responseBody
  -- pure x
  -- refreshAccessToken mgr oa rtkn

test2 = do
  BS.putStrLn $ serializeURIRef' $ ZO.authorizationUrl [Scope "ZohoCRM.modules.ALL", Scope "ZohoCRM.settings.ALL"] zohoOAuth
  BS.putStrLn "Please enter exchange token:"
  t <- Prelude.getLine
  mgr <- zohoManager
  O.fetchAccessToken2 mgr zohoOAuth (ExchangeToken $ toS t)
