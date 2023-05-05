{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Zoho.Books.Expense where

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
import Network.HTTP.Types as HT
import qualified Data.ByteString as BS

newtype ExpenseId = ExpenseId {rawExpenseId :: Text} 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text

data ExpenseLineItem = ExpenseLineItem 
  { eliLineItemId :: !(Maybe Text)
  , eliAccountId :: !(Maybe AccountId)
  , eliDescription :: !(Maybe Text)
  , eliAmount :: !(Maybe Double)
  , eliTaxId :: !(Maybe TaxId)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''ExpenseLineItem)

instance ToJSON ExpenseLineItem where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON ExpenseLineItem where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data Expense cf = Expense
  { expExpenseId :: !(Maybe ExpenseId)
  , expAccountId :: !(Maybe AccountId)
  , expDate :: !(Maybe Day)
  , expAmount :: !(Maybe Double)
  , expTaxId :: !(Maybe Text) -- TODO: Change to TaxId?
  , expSourceOfSupply :: !(Maybe Text) -- TODO: Change to GstStateCode
  , expDestinationOfSupply :: !(Maybe Text)
  , expHsnOrSac :: !(Maybe Text)
  , expGstNo :: !(Maybe Text)
  , expGstTreatment :: !(Maybe Text)
  , expLineItems :: !(Maybe [ExpenseLineItem])
  , expIsInclusiveTax :: !(Maybe Bool)
  , expReferenceNumber :: !(Maybe Text)
  , expDescription :: !(Maybe Text)
  , expCustomerId :: !(Maybe CustomerId)
  , expCurrencyId :: !(Maybe CurrencyId)
  , expExchangeRate :: !(Maybe Double)
  , expVendorId :: !(Maybe VendorId)
  , expPaidThroughAccountId :: !(Maybe AccountId)
  , expCustomFields :: !(Maybe [CustomField])
  , expOtherFields :: !(Maybe cf)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''Expense)

instance (ToJSON cf) => ToJSON (Expense cf) where
  toJSON :: ToJSON cf => Expense cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{expOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (expOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ HML.delete "other_fields" o
      _ -> y

instance (FromJSON cf) => FromJSON (Expense cf) where
  parseJSON v = do
    acc :: Expense OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc{expOtherFields=cf}

-- instance ToJSON Expense where
--   toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

-- instance FromJSON Expense where
--   parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

createRequest :: (ToJSON cf) => OrgId -> Expense cf -> Request
createRequest orgId exp =
  let params = Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/expenses") params [] exp

create :: forall m cf . (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> Expense cf -> m (Either Error (Expense cf))
create orgId exp = do
  (ZM.runRequestAndParseResponse $ createRequest orgId exp) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "expense" (Expense cf)) -> pure $ Right $ unwrapResponse r



deleteRequest :: OrgId -> ExpenseId -> Request
deleteRequest orgId ExpenseId{rawExpenseId} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareDelete (Common.mkApiEndpoint $ "/expenses/" <> toS rawExpenseId) params [] Nothing

delete :: (HasZoho m) => OrgId -> ExpenseId -> m (Either Error DeleteResult)
delete orgId eid = 
  ZM.runRequestAndParseResponse $  deleteRequest orgId eid

data ListOpts = ListOpts
  { optReferenceNumber :: !(Maybe ListOp)
  , optAccountName :: !(Maybe ListOp)
  , optCustomerName :: !(Maybe ListOp)
  , optVendorName :: !(Maybe ListOp)
  , optCustomerId :: !(Maybe CustomerId)
  , optVendorId :: !(Maybe VendorId)
  , optPaidThroughAccountId :: !(Maybe AccountId)
  , optSearchText :: !(Maybe Text)
  , optDateBefore :: !(Maybe Day)
  , optDateAfter :: !(Maybe Day)
  , optDateStart :: !(Maybe Day)
  , optDateEnd :: !(Maybe Day)
  , optPage :: !(Maybe Int)
  , optPerPage :: !(Maybe Int)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''ListOpts)

listRequest :: OrgId -> ListOpts -> Request
listRequest orgId ListOpts{..} =
  let params =  ZO.applyOptionalQueryParam "date_before" (show <$> optDateBefore) $
                ZO.applyOptionalQueryParam "date_after" (show <$> optDateAfter) $
                ZO.applyOptionalQueryParam "date_start" (show <$> optDateStart) $
                ZO.applyOptionalQueryParam "date_end" (show <$> optDateEnd) $
                ZO.applyOptionalQueryParam "search_text" optSearchText $
                ZO.applyOptionalQueryParam "paid_through_account_id" (rawAccountId <$> optPaidThroughAccountId) $
                ZO.applyOptionalQueryParam "vendor_id" (rawVendorId <$> optVendorId) $
                ZO.applyOptionalQueryParam "customer_id" (rawCustomerId <$> optCustomerId) $
                applyOptionalListOp "vendor_name" optVendorName $
                applyOptionalListOp "customer_name" optCustomerName $
                applyOptionalListOp "account_name" optAccountName $
                applyOptionalListOp "reference_number" optReferenceNumber $
                ZO.applyOptionalQueryParam "page" (show <$> optPage) $
                ZO.applyOptionalQueryParam "per_page" (show <$> optPerPage) $
                Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint $ "/expenses") params []

list :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "expenses" [Expense cf]))
list orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest orgId opts