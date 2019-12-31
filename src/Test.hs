module Test where

import Zoho.Types
import Network.OAuth.OAuth2 as O
import Zoho.OAuth as ZO
import Data.String.Conv
import Zoho.CRM.Records as R
import URI.ByteString
import URI.ByteString.QQ
import Network.HTTP.Client (Manager, ManagerSettings(..), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Network.Wreq as W
import Control.Lens
import Zoho.CRM.Contacts as Contacts
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Data.Time
import Data.Text as Text
import Data.Aeson.TH
import GHC.Generics
import Zoho.ZohoM as ZohoM

zohoOAuth :: OAuth2
zohoOAuth = mkOAuth hostUS (ClientId "1000.PCRP10N4ZKXC7F029BTTP6UT594BIH") (ClientSecret "67d211c3cb5c31df1a1899462514fba3abe152f6cb") ([uri|http://master.hetzner.vacationlabs.com/lambda/oauth-redirect|])

zohoManager :: IO Manager
zohoManager = newManager $ tlsManagerSettings{managerModifyRequest=logRequest}


data MyFields = MyFields
  { mfLeadStage :: Maybe Text
  } deriving (Eq, Show, Generic)

$(deriveJSON (Casing.aesonPrefix pascalSnakeCase) ''MyFields)
$(makeLensesWith abbreviatedFields ''MyFields)

-- -- test :: IO (Maybe (Either String (PaginatedResponse "data" [Contact Aeson.Value])))
-- -- test :: IO (W.Response BSL.ByteString)
-- -- test :: IO ()
-- -- test :: IO (Maybe (Either String (Maybe (Contact Aeson.Value))))
-- test = do
--   let rtkn = RefreshToken "1000.7950f276ab5889010ba61d5074835d16.84a6e76f73e09303f32e408c5ccb298f"
--   mgr <- zohoManager
--   t <- getCurrentTime
--   -- x <- withAccessToken mgr zohoOAuth rtkn Nothing (Contacts.list defaultListOptions{optPerPage=(Just 200), optModifiedAfter=(Just t{utctDayTime=75600})})
--   -- x <- withAccessToken mgr zohoOAuth rtkn Nothing (R.getSpecificRecord "Contacts" "3064310000023326001")
--   let c :: Contact MyFields = emptyContact
--         & fixedFields ?~ emptyContactFixedFields
--         & fixedFields._Just.lastName ?~ ("Nanda" :: Text)
--         & otherFields ?~ (MyFields {mfLeadStage = Just "Not Contacted"})
--   x <- withAccessToken mgr zohoOAuth rtkn Nothing (R.insert "Contacts" [c])
--   -- pure $ x ^? _Right . _1 . W.responseBody
--   pure x
--   -- refreshAccessToken mgr oa rtkn

-- test2 = do
--   BS.putStrLn $ serializeURIRef' $ ZO.authorizationUrl [Scope "ZohoCRM.modules.ALL", Scope "ZohoCRM.settings.ALL"] zohoOAuth
--   BS.putStrLn "Please enter exchange token:"
--   t <- Prelude.getLine
--   mgr <- zohoManager
--   O.fetchAccessToken2 mgr zohoOAuth (ExchangeToken $ toS t)

-- -- contact :: Contact ()
-- -- contact = Contact
-- --   { contactVisitSummary = Nothing
-- --   , contactScoreSummary = Nothing
-- --   , contactGoogleAdsInfo = Nothing
-- --   , contactSpecialFields = Nothing
-- --   , contactFixedFields = ContactFixedFields
-- --     { cffLastName = "Something"
-- --     }
-- --   }

test3 :: IO (Either Error (PaginatedResponse "data" [Contact ()]))
test3 = do
  let rtkn = RefreshToken "1000.7950f276ab5889010ba61d5074835d16.84a6e76f73e09303f32e408c5ccb298f"
  mgr <- zohoManager
  runZohoT mgr  zohoOAuth rtkn Nothing $ do
    -- _ <- ZohoM.refreshAccessToken
    -- r <- getRefreshToken
    -- a <- getAccessToken
    -- pure (a, r)
    R.list "Contacts2" defaultListOptions{ optPerPage = Just 5 }
