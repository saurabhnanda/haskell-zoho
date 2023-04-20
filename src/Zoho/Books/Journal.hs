{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Zoho.Books.Journal where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Control.Lens
import GHC.Generics
import Zoho.Types
import Zoho.Books.Common
import qualified Zoho.OAuth as ZO
import Zoho.ZohoM as ZM
import qualified Zoho.Books.Common as Common
import Network.HTTP.Client as HC (Request)
import Data.String.Conv
import qualified Data.HashMap.Lazy as HML
import Zoho.Books.Account (AccountId(..))
import Zoho.Books.Contact (VendorId (..), CustomerId (..))
import Data.Time
import Zoho.CRM.Common (ZohoResult(..))
import Network.HTTP.Types as HT
import qualified Data.ByteString as BS

newtype JournalId = JournalId {rawJournalId :: Text} 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text

data DebitCredit = Debit | Credit deriving (Eq, Show, Generic)

instance ToJSON DebitCredit where
  toJSON x = case x of
    Debit -> "debit"
    Credit -> "credit"

instance FromJSON DebitCredit where
  parseJSON = withText "Expecting a Text to parse into DebitCredit" $ \t -> do
    case t of
      "debit" -> pure Debit
      "Debit" -> pure Debit
      "credit" -> pure Credit
      "Credit" -> pure Credit
      _ -> fail  $ "Unexepcted value for DebitCredit: " <> toS t

data JournalType = Cash | Both deriving (Eq, Show, Generic)

instance ToJSON JournalType where
  toJSON x = case x of
    Cash -> "cash"
    Both -> "both"

instance FromJSON JournalType where
  parseJSON = withText "Expecting a Text to parse into JournalType" $ \t -> do
    case t of
      "cash" -> pure Cash
      "both" -> pure Both
      _ -> fail $ "Unexepcted value for JournalType: " <> toS t

data JournalLineItem = JournalLineItem 
  { jliAccountId :: !(Maybe AccountId)
  , jliCustomerId :: !(Maybe Text)
  , jliLineId :: !(Maybe Text)
  , jliDescription :: !(Maybe Text)
  , jliAmount :: !(Maybe Double)
  , jliDebitOrCredit :: !(Maybe DebitCredit)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''JournalLineItem)

instance ToJSON JournalLineItem where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON JournalLineItem where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data Journal cf = Journal
  { jJournalId :: !(Maybe JournalId)
  , jJournalDate :: !(Maybe Day)
  , jReferenceNumber :: !(Maybe Text)
  , jLineItems :: !(Maybe [JournalLineItem])
  , jCustomFields :: !(Maybe [CustomField])
  , jOtherFields :: !(Maybe cf)
  , jNotes :: !(Maybe Text)
  , jJournalType :: !(Maybe JournalType)
  , jEntryNumber :: !(Maybe Text)
  , jJournalNumberPrefix :: !(Maybe Text)
  , jJournalNumberSuffix :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)
$(makeLensesWith abbreviatedFields ''Journal)

instance (ToJSON cf) => ToJSON (Journal cf) where
  toJSON :: ToJSON cf => Journal cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{jOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (jOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ HML.delete "other_fields" o
      _ -> y

instance (FromJSON cf) => FromJSON (Journal cf) where
  parseJSON v = do
    acc :: Journal OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc{jOtherFields=cf}

-- instance ToJSON Expense where
--   toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

-- instance FromJSON Expense where
--   parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

createRequest :: (ToJSON cf) => OrgId -> Journal cf -> Request
createRequest orgId obj =
  let params = Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/journals") params [] obj

create :: forall m cf . (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> Journal cf -> m (Either Error (Journal cf))
create orgId obj = do
  (ZM.runRequestAndParseResponse $ createRequest orgId obj) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "journal" (Journal cf)) -> pure $ Right $ unwrapResponse r


deleteRequest :: OrgId -> JournalId -> Request
deleteRequest orgId JournalId{rawJournalId} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareDelete (Common.mkApiEndpoint $ "/journals/" <> toS rawJournalId) params [] Nothing

delete :: (HasZoho m) => OrgId -> JournalId -> m (Either Error DeleteResult)
delete orgId objId = 
  ZM.runRequestAndParseResponse $  deleteRequest orgId objId

data ListOpts = ListOpts
  { optReferenceNumber :: !(Maybe ListOp)
  , optEntryNumber :: !(Maybe ListOp)
  , optDateBefore :: !(Maybe Day)
  , optDateAfter :: !(Maybe Day) 
  , optDateStart :: !(Maybe Day) 
  , optDateEnd :: !(Maybe Day) 
  , optNotes :: !(Maybe ListOp)
  , optVendorId :: !(Maybe VendorId)
  , optCustomerId :: !(Maybe CustomerId)
  , optPage :: !(Maybe Int) 
  , optPerPage :: !(Maybe Int)

  -- , optAccountName :: !(Maybe ListOp)
  -- , optCustomerName :: !(Maybe ListOp)
  -- , optCustomerId :: !(Maybe CustomerId)
  -- , optPaidThroughAccountId :: !(Maybe AccountId)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''ListOpts)

listRequest :: OrgId -> ListOpts -> Request
listRequest orgId ListOpts{..} =
  let params =  ZO.applyOptionalQueryParam "date_before" (show <$> optDateBefore) $
                ZO.applyOptionalQueryParam "date_after" (show <$> optDateAfter) $
                ZO.applyOptionalQueryParam "date_start" (show <$> optDateStart) $
                ZO.applyOptionalQueryParam "date_end" (show <$> optDateEnd) $
                ZO.applyOptionalQueryParam "vendor_id" (rawVendorId <$> optVendorId) $
                ZO.applyOptionalQueryParam "customer_id" (rawCustomerId <$> optCustomerId) $
                applyOptionalListOp "entry_number" optEntryNumber $
                applyOptionalListOp "notes" optNotes $
                applyOptionalListOp "reference_number" optReferenceNumber $
                ZO.applyOptionalQueryParam "page" (show <$> optPage) $
                ZO.applyOptionalQueryParam "per_page" (show <$> optPerPage) $
                Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint "/journals") params []

list :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "journals" [Journal cf]))
list orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest orgId opts

fetchRequest :: OrgId -> JournalId -> Request
fetchRequest orgId JournalId{rawJournalId} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint $ "/journals/" <> toS rawJournalId) params []

fetch :: (HasZoho m, FromJSON cf) => OrgId -> JournalId -> m (Either Error (Journal cf))
fetch orgId objId = 
  ZM.runRequestAndParseResponse $ fetchRequest orgId objId
