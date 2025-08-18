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
import Network.HTTP.Client as HC (Request, path, method)
import Data.String.Conv
import qualified Data.Aeson.KeyMap as KeyMap
import Zoho.Books.Account (AccountId(..))
import Zoho.Books.Contact (VendorId (..), CustomerId (..))
import Zoho.Books.Item (ItemId (..))
import Data.Time
import Zoho.CRM.Common (ZohoResult(..))
import Network.HTTP.Types as HT
import qualified Data.ByteString as BS
import URI.ByteString as U

newtype InvoiceId = InvoiceId {rawInvoiceId :: Text} 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text

data DiscountType = ItemLevel | EntityLevel deriving (Eq, Show, Generic)

instance ToJSON DiscountType where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase){constructorTagModifier=Casing.snakeCase}

instance FromJSON DiscountType where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase){constructorTagModifier=Casing.snakeCase}

data InvoiceLineItem = InvoiceLineItem 
  { iliLineItemId :: !(Maybe Text)
  , iliItemId :: !(Maybe ItemId)
  , iliName :: !(Maybe Text)
  , iliAccountId :: !(Maybe AccountId)
  , iliDescription :: !(Maybe Text)
  , iliRate :: !(Maybe Double)
  , iliHsnOrSac :: !(Maybe Text)
  , iliQuantity :: !(Maybe Double)
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
  { invInvoiceId :: !(Maybe InvoiceId)
  , invIsPreGst :: !(Maybe Bool)
  , invPlaceOfSupply :: !(Maybe Text) -- GST State code?
  , invGstNo :: !(Maybe Text) 
  , invGstTreatment :: !(Maybe Text) -- TODO
  , invDate :: !(Maybe Day)
  , invPaymentTerms :: !(Maybe Int)
  , invPaymentTermsLabel :: !(Maybe Text)
  , invDueDate :: !(Maybe Day)
  , invInvoiceNumber :: !(Maybe Text)
  , invReferenceNumber :: !(Maybe Text)
  , invCustomerId :: !(Maybe CustomerId)
  , invCustomerName :: !(Maybe Text)
  , invCurrencyId :: !(Maybe CurrencyId)
  , invDiscount :: !(Maybe Double)
  , invIsDiscountBeforeTax :: !(Maybe Bool)
  , invDiscountType :: !(Maybe DiscountType)
  , invIsInclusiveTax :: !(Maybe Bool)
  , invLineItems :: !(Maybe [InvoiceLineItem])
  , invSalespersonName :: !(Maybe Text)
  , invCustomFields :: !(Maybe [CustomField])
  , invNotes :: !(Maybe Text)
  , invTerms :: !(Maybe Text)
  , invAdjustment :: !(Maybe Double)
  , invAdjustmentDescription :: !(Maybe Text)
  , invReason :: !(Maybe Text)
  , invTaxId :: !(Maybe TaxId)
  , invOtherFields :: !(Maybe cf)
  , invTdsPercent :: !(Maybe Text)
  , invTdsAmount :: !(Maybe (UnsafeEither Double Text))
  , invIsTdsAmountInPercent :: !(Maybe Bool)
  , invTdsTaxId :: !(Maybe TdsId)
  , invTotal :: !(Maybe Double)
  , invBalance :: !(Maybe Double)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''Invoice)

instance (ToJSON cf) => ToJSON (Invoice cf) where
  toJSON :: ToJSON cf => Invoice cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{invOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (invOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ KeyMap.delete "other_fields" o
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
               ZO.applyOptionalQueryParam "ignore_auto_number_generation" ((T.toLower . toS . show) <$> createIgnoreAutoNumberGeneration) $
               Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/invoices") params [] obj


create :: forall m cf . (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> CreateOpts -> Invoice cf -> m (Either Error (Invoice cf))
create orgId opts obj = do
  (ZM.runRequestAndParseResponse $ createRequest orgId opts obj) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "invoice" (Invoice cf)) -> pure $ Right $ unwrapResponse r

update :: forall m cf . (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> CreateOpts -> InvoiceId -> Invoice cf -> m (Either Error (Invoice cf))
update orgId opts InvoiceId{rawInvoiceId} obj = do
  let req = createRequest orgId opts obj
      p = toS $ U.serializeURIRef' $ Common.mkApiEndpoint $ "/invoices/" <> toS rawInvoiceId
      finalReq = req{method="PUT", path=p}
  (ZM.runRequestAndParseResponse finalReq) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "invoice" (Invoice cf)) -> pure $ Right $ unwrapResponse r
  
fetchRequest :: OrgId -> InvoiceId -> Request
fetchRequest orgId InvoiceId{rawInvoiceId} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint $ "/invoices/" <> toS rawInvoiceId) params []

fetch :: forall m cf . (HasZoho m, FromJSON cf) => OrgId -> InvoiceId -> m (Either Error (Invoice cf))
fetch orgId objId = 
  (ZM.runRequestAndParseResponse $  fetchRequest orgId objId) >>= \case
    Left e -> pure $ Left e
    Right (inv :: ResponseWrapper "invoice" (Invoice cf)) -> pure $ Right $ unwrapResponse inv

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


markAsSentRequest :: OrgId -> InvoiceId -> Request
markAsSentRequest orgId InvoiceId{..} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint $ "/invoices/" <> toS rawInvoiceId <> "/status/sent") params [] Aeson.Null


markAsSent :: (HasZoho m) => OrgId -> InvoiceId -> m (Either Error OperationResult)
markAsSent orgId invid = 
  ZM.runRequestAndParseResponse $ markAsSentRequest orgId invid
