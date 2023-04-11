{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Zoho.Books.VendorPayment where

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
import Zoho.Books.Bill (BillId(..))

newtype VendorPaymentId = VendorPaymentId {rawVendorPaymentId :: Text} 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text

data VendorPaymentLineItem = VenderPaymentLineItem 
  { vpliBillPaymentId :: !(Maybe VendorPaymentId)
  , vpliVendorPaymentId :: !(Maybe VendorPaymentId)
  , vpliAmountApplied :: !(Maybe Double)
  , vpliTaxAmountWithheld :: !(Maybe Text)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''VendorPaymentLineItem)

instance ToJSON VendorPaymentLineItem where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON VendorPaymentLineItem where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data VendorPayment cf = VendorPayment
  { vpPaymentId :: !(Maybe VendorPaymentId)
  , vpVendorId :: !(Maybe VendorId)
  , vpBills :: !(Maybe [VendorPaymentLineItem])
  , vpDate :: !(Maybe Day)
  , vpAmount :: !(Maybe Double)
  , vpPaidThroughAccountId :: !(Maybe AccountId)
  , vpPaymentMode :: !(Maybe Text)
  , vpDescription :: !(Maybe Text)
  , vpReferenceNumber :: !(Maybe Text)
  , vpCustomFields :: !(Maybe [CustomField])
  , vpOtherFields :: !(Maybe cf)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)
$(makeLensesWith abbreviatedFields ''VendorPayment)

instance (ToJSON cf) => ToJSON (VendorPayment cf) where
  toJSON :: ToJSON cf => VendorPayment cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{vpOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (vpOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ HML.delete "other_fields" o
      _ -> y

instance (FromJSON cf) => FromJSON (VendorPayment cf) where
  parseJSON v = do
    acc :: VendorPayment OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc{vpOtherFields=cf}

-- instance ToJSON Expense where
--   toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

-- instance FromJSON Expense where
--   parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

createRequest :: (ToJSON cf) => OrgId -> VendorPayment cf -> Request
createRequest orgId obj =
  let params = Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/vendorpayments") params [] obj

create :: forall m cf . (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> VendorPayment cf -> m (Either Error (VendorPayment cf))
create orgId obj = do
  (ZM.runRequestAndParseResponse $ createRequest orgId obj) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "payment" (VendorPayment cf)) -> pure $ Right $ unwrapResponse r


deleteRequest :: OrgId -> VendorPaymentId -> Request
deleteRequest orgId VendorPaymentId{rawVendorPaymentId} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareDelete (Common.mkApiEndpoint $ "/vendorpayments/" <> toS rawVendorPaymentId) params [] Nothing

delete :: (HasZoho m) => OrgId -> VendorPaymentId -> m (Either Error DeleteResult)
delete orgId objId = 
  ZM.runRequestAndParseResponse $  deleteRequest orgId objId

data ListOpts = ListOpts
  { optReferenceNumber :: !(Maybe ListOp)
  , optPaymentNumber :: !(Maybe ListOp)
  , optDateBefore :: !(Maybe Day)
  , optDateAfter :: !(Maybe Day) 
  , optDateStart :: !(Maybe Day) 
  , optDateEnd :: !(Maybe Day)
  , optNotes :: !(Maybe ListOp)
  , optDescription :: !(Maybe ListOp)
  , optVendorName :: !(Maybe ListOp) 
  , optVendorId :: !(Maybe VendorId)
  , optBillId :: !(Maybe BillId)
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
                ZO.applyOptionalQueryParam "vendor_id" (rawVendorId <$> optVendorId) $
                ZO.applyOptionalQueryParam "bill_id" (rawBillId <$> optBillId) $
                applyOptionalListOp "vendor_name" optVendorName $
                applyOptionalListOp "description" optDescription $
                applyOptionalListOp "notes" optNotes $
                applyOptionalListOp "reference_number" optReferenceNumber $
                applyOptionalListOp "payment_number" optPaymentNumber $
                ZO.applyOptionalQueryParam "page" (show <$> optPage) $
                ZO.applyOptionalQueryParam "per_page" (show <$> optPerPage) $
                Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint "/vendorpayments") params []

list :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "vendorpayments" [VendorPayment cf]))
list orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest orgId opts