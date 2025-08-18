{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Zoho.Books.CreditNote where

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
import qualified Data.Aeson.KeyMap as KeyMap
import Zoho.Books.Account (AccountId(..))
import Zoho.Books.Contact (VendorId (..), CustomerId (..))
import Data.Time
import Zoho.CRM.Common (ZohoResult(..))
import Network.HTTP.Types as HT
import qualified Data.ByteString as BS
import Zoho.Books.Item (ItemId)
import Zoho.Books.Invoice (DiscountType, InvoiceId(..))

newtype CreditNoteId = CreditNoteId { rawCreditNoteId :: Text} 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text

data CreditNoteLineItem = CreditNoteLineItem 
  { cnliLineItemId :: !(Maybe Text)
  , cnliItemId :: !(Maybe ItemId)
  , cnliDescription :: !(Maybe Text)
  , cnliName :: !(Maybe Text)
  , cnliAccountId :: !(Maybe AccountId)
  , cnliQuantity :: !(Maybe Double)
  , cnliRate :: !(Maybe Double)
  , cnliTaxId :: !(Maybe TaxId)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''CreditNoteLineItem)

instance ToJSON CreditNoteLineItem where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON CreditNoteLineItem where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data CreditNote cf = CreditNote
  { cnCreditnoteId :: !(Maybe CreditNoteId)
  , cnCustomerId :: !(Maybe CustomerId)
  , cnDate :: !(Maybe Day)
  , cnLineItems :: !(Maybe [CreditNoteLineItem])
  , cnCreditnoteNumber :: !(Maybe Text)
  , cnGstTreatment :: !(Maybe Text) -- TODO
  , cnGstNo :: !(Maybe Text) 
  , cnPlaceOfSupply :: !(Maybe Text) -- TODO: Change to GstStateCode
  -- , cnIgnoreAutoNumberGeneration :: !(Maybe Bool)
  , cnReferenceNumber :: !(Maybe Text)
  , cnCustomFields :: !(Maybe [CustomField])
  , cnOtherFields :: !(Maybe cf)
  , cnNotes :: !(Maybe Text)

  -- , cnDestinationOfSupply :: !(Maybe Text) -- TODO: Change to GstStateCode
  -- , cnIsInclusiveTax :: !(Maybe Bool)
  , cnAdjustmentDescription :: !(Maybe Text)
  , cnAdjustment :: !(Maybe Double)
  , cnCurrencyId :: !(Maybe CurrencyId)
  , cnInvoiceId :: !(Maybe Text)
  , cnReferenceInvoiceType :: !(Maybe Text)
  , cnReasonForCreditNote :: !(Maybe Text)
  , cnDiscount :: !(Maybe (UnsafeEither Double Text))
  , cnIsDiscountBeforeTax :: !(Maybe Bool)
  , cnDiscountType :: !(Maybe DiscountType)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)
$(makeLensesWith abbreviatedFields ''CreditNote)

instance (ToJSON cf) => ToJSON (CreditNote cf) where
  toJSON :: ToJSON cf => CreditNote cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{cnOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (cnOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ KeyMap.delete "other_fields" o
      _ -> y

instance (FromJSON cf) => FromJSON (CreditNote cf) where
  parseJSON v = do
    acc :: CreditNote OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc{cnOtherFields=cf}

createRequest :: (ToJSON cf) => OrgId -> CreditNote cf -> Request
createRequest orgId obj =
  let params = Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/creditnotes") params [] obj

create :: forall m cf . (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> CreditNote cf -> m (Either Error (CreditNote cf))
create orgId obj = do
  (ZM.runRequestAndParseResponse $ createRequest orgId obj) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "creditnote" (CreditNote cf)) -> pure $ Right $ unwrapResponse r


deleteRequest :: OrgId -> CreditNoteId -> Request
deleteRequest orgId CreditNoteId{rawCreditNoteId} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareDelete (Common.mkApiEndpoint $ "/creditnotes/" <> toS rawCreditNoteId) params [] Nothing

delete :: (HasZoho m) => OrgId -> CreditNoteId -> m (Either Error DeleteResult)
delete orgId objId = 
  ZM.runRequestAndParseResponse $  deleteRequest orgId objId

data ListOpts = ListOpts
  { optCreditnoteNumber :: !(Maybe Text)
  , optDateBefore :: !(Maybe Day)
  , optDateAfter :: !(Maybe Day)
  , optDateStart :: !(Maybe Day)
  , optDateEnd :: !(Maybe Day)
  , optReferenceNumber :: !(Maybe ListOp)
  , optSearchText :: !(Maybe Text)

  -- , optBillNumber :: !(Maybe ListOp)
  -- , optDescription :: !(Maybe ListOp)
  -- , optVendorId :: !(Maybe VendorId)
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
                ZO.applyOptionalQueryParam "creditnote_number" optCreditnoteNumber $
                applyOptionalListOp "reference_number" optReferenceNumber $
                ZO.applyOptionalQueryParam "page" (show <$> optPage) $
                ZO.applyOptionalQueryParam "per_page" (show <$> optPerPage) $
                Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint "/creditnotes") params []

list :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "creditnotes" [CreditNote cf]))
list orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest orgId opts

data CreditNoteInvoice = CreditNoteInvoice
  { cniInvoiceId :: !(Maybe InvoiceId)
  , cniAmountApplied :: !(Maybe Double)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)
$(makeLensesWith abbreviatedFields ''CreditNoteInvoice)

instance ToJSON CreditNoteInvoice where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON CreditNoteInvoice where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)


applyToInvoicesRequest :: OrgId -> CreditNoteId -> [CreditNoteInvoice] -> Request
applyToInvoicesRequest orgId CreditNoteId{..} vcbs = 
  let params = Common.orgIdParam orgId
      pload = ResponseWrapper vcbs :: ResponseWrapper "invoices" [CreditNoteInvoice]
  in ZO.prepareJSONPost (Common.mkApiEndpoint $ "/creditnotes/" <> toS rawCreditNoteId <> "/invoices") params [] pload

applyToInvoices :: (HasZoho m) => OrgId -> CreditNoteId -> [CreditNoteInvoice] -> m (Either Error [CreditNoteInvoice])
applyToInvoices orgId cnId vcbs = do
  (ZM.runRequestAndParseResponse $ applyToInvoicesRequest orgId cnId vcbs) >>= \case
    Left e -> 
      pure $ Left e
    Right (r :: ResponseWrapper "apply_to_invoices" (ResponseWrapper "invoices" [CreditNoteInvoice])) -> 
      pure $ Right $ unwrapResponse $ unwrapResponse r


data CreditNoteRefund = CreditNoteRefund
  { refDate :: !(Maybe Day)
  , refRefundMode :: !(Maybe Text)
  , refReferenceNumber :: !(Maybe Text)
  , refAmount :: !(Maybe Double)
  , refFromAccountId :: !(Maybe AccountId)
  , refDescription :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)
$(makeLensesWith abbreviatedFields ''CreditNoteRefund)

instance ToJSON CreditNoteRefund where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON CreditNoteRefund where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

creditNoteRefundRequest :: OrgId -> CreditNoteId -> CreditNoteRefund -> Request
creditNoteRefundRequest orgId CreditNoteId{..} obj =
  let params = Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint $ "/creditnotes/" <> toS rawCreditNoteId <> "/refunds") params [] obj

creditNoteRefund :: (HasZoho m) => OrgId -> CreditNoteId -> CreditNoteRefund -> m (Either Error CreditNoteRefund)
creditNoteRefund orgId cnId obj = do
  (ZM.runRequestAndParseResponse $ creditNoteRefundRequest orgId cnId obj) >>= \case
    Left e ->
      pure $ Left e
    Right (r :: ResponseWrapper "creditnote_refund" CreditNoteRefund) ->
      pure $ Right $ unwrapResponse r
