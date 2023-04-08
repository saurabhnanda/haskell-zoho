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

newtype ExpenseId = ExpenseId {rawExpenseId :: Text} 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text

newtype TaxId = TaxId {rawTaxId :: Text}
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text
  deriving (FromJSON) via Text


newtype CurrencyId = CurrencyId {rawCurrencyId :: Text}
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text
  deriving (FromJSON) via Text

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
  , expLineItems :: !(Maybe [ExpenseLineItem])
  , expIsInclusiveTax :: !(Maybe Bool)
  , expReferenceNumber :: !(Maybe Text)
  , expDescription :: !(Maybe Text)
  , expCustomerId :: !(Maybe CustomerId)
  , expCurrencyId :: !(Maybe CurrencyId)
  , expExchangeRate :: !(Maybe Double)
  , expVendorId :: !(Maybe VendorId)
  , expPaidThroughAccountId :: !(Maybe AccountId)
  , expOtherFields :: !(Maybe cf)
  } deriving (Eq, Show, Generic)

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
