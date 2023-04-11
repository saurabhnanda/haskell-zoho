{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Zoho.Books.Invoice where

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

newtype InvoiceId = InvoiceId {rawInvoiceId :: Text} 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text

data InvoiceLineItem = InvoiceLineItem 
  { iliLineItemId :: !(Maybe Text)
  , iliItemId :: !(Maybe Text)
  , iliName :: !(Maybe Text)
  , iliAccountId :: !(Maybe AccountId)
  , iliDescription :: !(Maybe Text)
  , iliRate :: !(Maybe Double)
  , iliHsnOrSac :: !(Maybe Text)
  , iliQuantity :: !(Maybe Double)
  , iliCustomerId :: !(Maybe CustomerId)
  , iliTaxId :: !(Maybe TaxId)
  , iliIsBillable :: !(Maybe Bool)
  , iliUnit :: !(Maybe Text)
  , iliDiscountAmount :: !(Maybe Double)
  , iliDiscount :: !(Maybe Double)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''InvoiceLineItem)

instance ToJSON InvoiceLineItem where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON InvoiceLineItem where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data Invoice cf = Invoice
  { inInvoiceId :: !(Maybe InvoiceId)
  , invIsPreGst :: !(Maybe Bool)
  , invPlaceOfSupply :: !(Maybe Text) -- GST State code?
  , invGstNo :: !(Maybe Text) 
  , invGstTreatment :: !(Maybe Text) -- TODO
  , invDate :: !(Maybe Day)
  , invPaymentTerms :: !(Maybe Int)
  , invPaymentTermsLabel :: !(Maybe Text)
  , invDueDate :: !(Maybe Day)
  , invReferenceNumber :: !(Maybe Text)
  , invCustomerId :: !(Maybe CustomerId)
  , invCustomerNamr :: !(Maybe Text)
  , invCurrencyId :: !(Maybe CurrencyId)
  , invDiscount :: !(Maybe Double)
  , invIsDiscountBeforeTax :: !(Maybe Bool)
  , invDiscountType :: !(Maybe Text)
  , invIsInclusiveTax :: !(Maybe Bool)
  , invLineItems :: !(Maybe [InvoiceLineItem])
  , invSalespersonName :: !(Maybe Text)
  , invCustomFields :: !(Maybe [CustomField])
  , invNotes :: !(Maybe Text)
  , invTerms :: !(Maybe Text)
  , invAdjustments :: !(Maybe Double)
  , invAdjustmentDescription :: !(Maybe Double)
  , invReason :: !(Maybe Text)
  , invTaxId :: !(Maybe TaxId)
  , invOtherFields :: !(Maybe cf)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''Invoice)

instance (ToJSON cf) => ToJSON (Invoice cf) where
  toJSON :: ToJSON cf => Invoice cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{invOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (invOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ HML.delete "other_fields" o
      _ -> y

instance (FromJSON cf) => FromJSON (Invoice cf) where
  parseJSON v = do
    acc :: Invoice OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc{invOtherFields=cf}

-- instance ToJSON Expense where
--   toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

-- instance FromJSON Expense where
--   parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data CreateOpts = CreateOpts
  { createSend :: !(Maybe Bool)
  , createIgnoreAutoNumberGeneration :: !(Maybe Bool)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''CreateOpts)

createRequest :: (ToJSON cf) => OrgId -> CreateOpts -> Invoice cf -> Request
createRequest orgId CreateOpts{..} obj =
  let params = ZO.applyOptionalQueryParam "send" ((T.toLower . toS . show) <$> createSend) $
               ZO.applyOptionalQueryParam "ignore_auto_number_generation" ((T.toLower . toS . show) <$> createSend) $
               Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/invoices") params [] obj


create :: forall m cf . (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> CreateOpts -> Invoice cf -> m (Either Error (Invoice cf))
create orgId opts obj = do
  (ZM.runRequestAndParseResponse $ createRequest orgId opts obj) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "invoice" (Invoice cf)) -> pure $ Right $ unwrapResponse r


deleteRequest :: OrgId -> InvoiceId -> Request
deleteRequest orgId InvoiceId{rawInvoiceId} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareDelete (Common.mkApiEndpoint $ "/invoices/" <> toS rawInvoiceId) params [] Nothing

delete :: (HasZoho m) => OrgId -> InvoiceId -> m (Either Error DeleteResult)
delete orgId objId = 
  ZM.runRequestAndParseResponse $  deleteRequest orgId objId

data ListOpts = ListOpts
  { optInvoiceNumber :: !(Maybe ListOp)
  , optItemName :: !(Maybe ListOp)
  -- , optItemId :: !(Maybe Text)
  , optItemDescription :: !(Maybe ListOp)
  , optReferenceNumber :: !(Maybe ListOp)
  , optCustomerName :: !(Maybe Text)
  , optEmail :: !(Maybe Text)
  , optCustomField :: !(Maybe ListOp)
  , optDateBefore :: !(Maybe Day)
  , optDateAfter :: !(Maybe Day)
  , optDateStart :: !(Maybe Day)
  , optDateEnd :: !(Maybe Day)
  , optDescription :: !(Maybe ListOp)
  , optCustomerId :: !(Maybe CustomerId)
  , optSearchText :: !(Maybe Text)
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
                ZO.applyOptionalQueryParam "search_text" optSearchText $
                ZO.applyOptionalQueryParam "customer_id" (rawCustomerId <$> optCustomerId) $
                applyOptionalListOp "invoice_number" optInvoiceNumber $
                applyOptionalListOp "item_name" optItemName $
                applyOptionalListOp "item_description" optItemDescription $
                applyOptionalListOp "reference_number" optReferenceNumber $
                applyOptionalListOp "custom_field" optCustomField $
                ZO.applyOptionalQueryParam "customer_name" optCustomerName $
                ZO.applyOptionalQueryParam "email" optEmail $
                ZO.applyOptionalQueryParam "page" (show <$> optPage) $
                ZO.applyOptionalQueryParam "per_page" (show <$> optPerPage) $
                Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint $ "/invoices") params []

list :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "invoices" [Invoice cf]))
list orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest orgId opts