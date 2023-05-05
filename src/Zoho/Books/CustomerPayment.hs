{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Zoho.Books.CustomerPayment where

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
import Zoho.Books.Invoice (InvoiceId(..))

newtype CustomerPaymentId = CustomerPaymentId {rawCustomerPaymentId :: Text} 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text

data CustomerPaymentInvoice = CustomerPaymentInvoice 
  { cpiInvoicePaymentId :: !(Maybe CustomerPaymentId)
  , cpiInvoiceId :: !(Maybe InvoiceId)
  , cpiAmountApplied :: !(Maybe Double)
  -- , cpiTaxAmountWithheld :: !(Maybe Text)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''CustomerPaymentInvoice)

instance ToJSON CustomerPaymentInvoice where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON CustomerPaymentInvoice where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data CustomerPayment cf = CustomerPayment
  { cpPaymentId :: !(Maybe CustomerPaymentId)
  , cpCustomerId :: !(Maybe CustomerId)
  , cpPaymentMode :: !(Maybe Text)
  , cpAmount :: !(Maybe Double)
  , cpDate :: !(Maybe Day)
  , cpReferenceNumber :: !(Maybe Text)
  , cpDescription :: !(Maybe Text)
  , cpInvoices :: !(Maybe [CustomerPaymentInvoice])
  , cpCustomFields :: !(Maybe [CustomField])
  , cpOtherFields :: !(Maybe cf)
  , cpAccountId :: !(Maybe AccountId)
  , cpPaymentNumberPrefix :: !(Maybe Text)
  , cpPaymentNumberSuffix :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)
$(makeLensesWith abbreviatedFields ''CustomerPayment)

instance (ToJSON cf) => ToJSON (CustomerPayment cf) where
  toJSON :: ToJSON cf => CustomerPayment cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{cpOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (cpOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ HML.delete "other_fields" o
      _ -> y

instance (FromJSON cf) => FromJSON (CustomerPayment cf) where
  parseJSON v = do
    acc :: CustomerPayment OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc{cpOtherFields=cf}

-- instance ToJSON Expense where
--   toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

-- instance FromJSON Expense where
--   parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data CreateOpts = CreateOpts
  { createIgnoreAutoNumberGeneration :: !(Maybe Bool)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)
$(makeLensesWith abbreviatedFields ''CreateOpts)

createRequest :: (ToJSON cf) => OrgId -> CreateOpts -> CustomerPayment cf -> Request
createRequest orgId CreateOpts{..} obj =
  let params =  ZO.applyOptionalQueryParam "ignore_auto_number_generation" ((T.toLower . toS . show) <$> createIgnoreAutoNumberGeneration) $
                Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/customerpayments") params [] obj

create :: forall m cf . (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> CreateOpts -> CustomerPayment cf -> m (Either Error (CustomerPayment cf))
create orgId opts obj = do
  (ZM.runRequestAndParseResponse $ createRequest orgId opts obj) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "payment" (CustomerPayment cf)) -> pure $ Right $ unwrapResponse r


deleteRequest :: OrgId -> CustomerPaymentId -> Request
deleteRequest orgId CustomerPaymentId{rawCustomerPaymentId} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareDelete (Common.mkApiEndpoint $ "/customerpayments/" <> toS rawCustomerPaymentId) params [] Nothing

delete :: (HasZoho m) => OrgId -> CustomerPaymentId -> m (Either Error DeleteResult)
delete orgId objId = 
  ZM.runRequestAndParseResponse $  deleteRequest orgId objId

data ListOpts = ListOpts
  { optCustomerName :: !(Maybe ListOp)
  , optReferenceNumber :: !(Maybe ListOp)
  , optDateBefore :: !(Maybe Day)
  , optDateAfter :: !(Maybe Day) 
  , optDateStart :: !(Maybe Day) 
  , optDateEnd :: !(Maybe Day)
  , optNotes :: !(Maybe ListOp)
  , optPaymentMode :: !(Maybe ListOp)
  , optSearchText :: !(Maybe Text)
  , optCustomerId :: !(Maybe CustomerId)
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
                ZO.applyOptionalQueryParam "customer_id" (rawCustomerId <$> optCustomerId) $
                applyOptionalListOp "customer_name" optCustomerName $
                applyOptionalListOp "notes" optNotes $
                applyOptionalListOp "reference_number" optReferenceNumber $
                applyOptionalListOp "payment_mode" optPaymentMode $
                ZO.applyOptionalQueryParam "page" (show <$> optPage) $
                ZO.applyOptionalQueryParam "per_page" (show <$> optPerPage) $
                Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint "/customerpayments") params []

list :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "customerpayments" [CustomerPayment cf]))
list orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest orgId opts