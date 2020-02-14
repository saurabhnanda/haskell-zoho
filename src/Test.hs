{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
module Test where

import Zoho.Types
import Network.OAuth.OAuth2 as O
import Zoho.OAuth as ZO
import Data.String.Conv
import Zoho.CRM.Records as R
import URI.ByteString
import URI.ByteString.QQ
import Network.HTTP.Client (Manager, ManagerSettings(..), newManager, redirectCount, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Control.Lens
import Zoho.CRM.Contacts as Contacts
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Data.Time
import Data.Text as Text
import Data.Aeson.TH
import GHC.Generics
import Zoho.ZohoM as ZohoM
import Zoho.CRM.Common.Utils
import Data.String (IsString(..))
import Network.HTTP.Client.TLS
import Network.Connection (TLSSettings(..))
import Control.Monad.IO.Class
import Zoho.Desk.Account as ZDA
import Zoho.Desk.Contact as ZDC
import Zoho.Desk.Common (HasCustomFields(..), SearchResults(..))
import Zoho.Desk.Ticket as ZDT
import Zoho.Desk.Thread as Thread
import Zoho.Desk.Common

zohoOAuth :: OAuth2
zohoOAuth = mkOAuth hostUS (ClientId "1000.PCRP10N4ZKXC7F029BTTP6UT594BIH") (ClientSecret "67d211c3cb5c31df1a1899462514fba3abe152f6cb") ([uri|http://master.hetzner.vacationlabs.com/lambda/oauth-redirect|])

deskRefreshToken :: RefreshToken
deskRefreshToken = RefreshToken {rtoken = "1000.31999e915bbc7581a2810685060e8e9e.93ae10b4a89045d644ccfacab2eeb79b"}

deskOrgId :: OrgId
deskOrgId = OrgId "104397711"

zohoManager :: IO Manager
zohoManager =
  let x = TLSSettingsSimple
          { settingDisableCertificateValidation = True
          , settingUseServerName = True
          , settingDisableSession = False
          }
      y = mkManagerSettings x Nothing
  in newManager y{managerModifyRequest=logRequest}


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


-- data VLContactFields = VLContactFields
--   { contactEmail :: !(Maybe Text)
--   } deriving (Eq, Show, Generic, EmptyZohoStructure)
-- $(makeLensesWith abbreviatedFields ''VLContactFields)
-- $(deriveJSON (zohoPrefix pascalSnakeCase) ''VLContactFields)

-- emptyVlContactFields :: VLContactFields
-- emptyVlContactFields = emptyZohoStructure

-- type VLContact = Contacts.Contact VLContactFields

-- myContact :: VLContact
-- myContact =
--   let cf = emptyVlContactFields & email ?~ "saurabh5@mailinator.com"
--   in Contacts.emptyContact
--        & otherFields ?~ cf
--        & Contacts.lastName ?~ "random shit"


--test3 :: IO (Maybe (Contact ()))
test3 = do
  let rtkn = RefreshToken "1000.7950f276ab5889010ba61d5074835d16.84a6e76f73e09303f32e408c5ccb298f"
  mgr <- zohoManager
  runZohoT mgr  zohoOAuth rtkn Nothing $ do
    Contacts.search @_ @() (SearchEmail "saurabh5@mailinator.com") emptySearchOpts
    -- Contacts.upsert [myContact] TSOmit ["Email"]

    -- _ <- ZohoM.refreshAccessToken
    -- r <- getRefreshToken
    -- a <- getAccessToken
    -- pure (a, r)
    -- R.list "Cases" defaultListOptions{ optPerPage = Just 5 }
--     r :: (Either Error (Maybe (PaginatedResponse "data" [Contact ()]))) <-
    -- Contact.upsert
    --   [ (emptyContact :: (Contact ()))
    --     & 
    --   ]
    --   TSOmit
    --   ["Email"]
    -- r <- Contacts.search @_ @() (SearchEmail "saurabh5@mailinator.com") emptySearchOpts

    -- case r ^? _Right . _Just . actualData . (ix 0) of
    --   Nothing -> Prelude.error "no search results"
    --   Just con ->
    --     Contacts.upsert [con{contactLastName=Just "ABCD", contactId=Nothing}] TSOmit ["Email"]

test = serializeURIRef' $ ZO.authorizationUrl (Scope <$> ["Desk.tickets.ALL", "Desk.contacts.ALL", "Desk.search.READ"]) zohoOAuth

-- test4 = do
--   mgr <- zohoManager
--   runZohoT mgr zohoOAuth deskRefreshToken Nothing $ do
--     -- ZDA.list @_ @() ZDA.emptyListOptions{optFrom=Just 1000} deskOrgId
--     let sopts = ZDA.emptySearchOptions{soptsCustomField1=Just ("cf_vl_client_id", "999999")}
--     (ZDA.search @_ @NoCustomFields sopts deskOrgId) >>= \case
--       Left e -> pure $ Left e
--       Right x -> case searchData x of
--         [] -> Prelude.error "unhandled"
--         a:_ -> pure $ Right a
    -- ZohoM.runRequest $ ZDA.searchRequest emptySearchOptions deskOrgId

    -- let r = ZO.prepareGet (ZO.mkEndpoint (Host "desk.zoho.com") "/api/v1/accounts/104785000000074190") [] [("orgId", deskOrgId)]
    -- res <- ZohoM.runRequest r{redirectCount=0}
    -- liftIO $ BSL.putStrLn $ responseBody res
    -- pure $ (eitherDecode $ responseBody res :: Either String (ZDA.Account Aeson.Value))

-- code = "1000.27551c6121b80c94a637c1811e1dfe2e.39a5589989851fc272d936fa0a45d024"


test5 = do
  mgr <- zohoManager
  runZohoT mgr zohoOAuth deskRefreshToken Nothing $ do
    -- ZDA.list @_ @() ZDA.emptyListOptions{optFrom=Just 1000} deskOrgId
    let sopts = ZDC.emptySearchOptions & ZDC.email ?~ "saurabhnanda@gmail.com"
    ZDC.search @_ @NoCustomFields sopts deskOrgId

test6 = do
  mgr <- zohoManager
  runZohoT mgr zohoOAuth deskRefreshToken Nothing $ do
    -- let sopts = ZDA.emptySearchOptions
    --             & customFields .~ [("cf_vl_client_id", "999999")]
    -- ZDT.list @_ @Aeson.Value ZDT.emptyListOptions deskOrgId
    -- tkt <- ZDT.create @_ @Aeson.Value deskOrgId $
    --   ZDT.emptyTicket
    --   & ZDT.subject ?~ "Test subject - please ignore"
    --   & ZDT.departmentId ?~ "104785000000006907"
    --   & ZDT.contactId ?~ "104785000001015077"
    --   & ZDT.priority ?~ "High"
    --   & customFields ?~ Aeson.object [ "cf_labels" Aeson..= ("billing-other, new-tag-required"::Text) ]
    -- Thread.create deskOrgId "104785000002472001" $
    --   Thread.emptyThread
    --   & Thread.channel ?~ "EMAIL"
    --   & Thread.to ?~ "saurabhnanda@gmail.com"
    --   & Thread.content ?~ "Some content comes here"
    --   & Thread.fromEmailAddress ?~ "support@vacationlabs.com"
    Thread.sendEmailReply
      deskOrgId
      "104785000002472001"
      "support@vacationlabs.com"
      "saurabhnanda+1@gmail.com"
      ["saurabhnanda+2@gmail.com", "saurabh@vacationlabs.com"]
      "random shit"
      Thread.emptyThread
      -- & Thread.contentType 
    -- let lopts = Thread.emptyListOptions
    --             & Thread.limit ?~ 5
    -- Thread.list deskOrgId lopts "104785000001546131"


-- findZohoAccount :: (HasZoho m)
--                 => OrgId
--                 -> m [Account Aeson.Value]
findZohoAccount orgId fn = do
  let sopts = fn ZDA.emptySearchOptions
              -- & customFields .~ [("cf_vl_client_id", toS $ show cid)]
  (ZDA.search sopts orgId) >>= \case
    Left e -> Prelude.error $ show e
    Right SearchResults{searchData} -> pure searchData
