{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Zoho.Books.Account where

-- import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Control.Lens
import GHC.Generics
import Data.Time
import Zoho.Types
-- import Zoho.Books.Common
import qualified Zoho.OAuth as ZO
import Zoho.ZohoM as ZM
import qualified Zoho.Books.Common as Common
import Network.HTTP.Client as HC (Request)
import qualified Data.HashMap.Lazy as HML
-- import Data.String.Conv

newtype AccountId = AccountId { rawAccountId :: Text } deriving (Eq, Show, Generic, Ord)

instance ToJSON AccountId where
  toJSON = toJSON . rawAccountId

instance FromJSON AccountId where
  parseJSON = withText "Expecting text to parse into AccountId" $ \t -> pure $ AccountId t

data AccountType = OtherAsset | OtherCurrentAsset | Cash | Bank | FixedAsset | OtherCurrentLiability | CreditCard | LongTermLiability | OtherLiability | Equity | Income | OtherIncome | Expense | CostOfGoodsSold | OtherExpense | AccountsReceivable | AccountsPayable deriving (Eq, Show, Generic, Ord, Enum, Bounded)

instance ToJSON AccountType where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON AccountType where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data CustomField = CustomField
  { cfCustomfieldId :: !Text
  , cfValue :: !Aeson.Value
  } deriving (Eq, Show, Generic)

instance ToJSON CustomField where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON CustomField where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data Account cf = Account
  { accAccountName :: !(Maybe Text)
  , accAccountCode :: !(Maybe Text)
  , accAccountType :: !(Maybe Text)
  , accCurrencyId :: !(Maybe Text)
  , accDescription :: !(Maybe Text)
  , accIsActive :: !(Maybe Bool)
  , accCurrencyCode :: !(Maybe Text)
  , accCustomFields :: !(Maybe [CustomField])
  , accParentAccountId :: !(Maybe AccountId)
  , accCreatedTime :: !(Maybe UTCTime)
  , accLastModifiedTime :: !(Maybe UTCTime)
  , accOtherFields :: !(Maybe cf)
  } deriving (Eq, Show, Generic)

instance (ToJSON cf) => ToJSON (Account cf) where
  toJSON :: ToJSON cf => Account cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{accOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (accOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ HML.delete "other_fields" o
      _ -> y

instance (FromJSON cf) => FromJSON (Account cf) where
  parseJSON v = do
    acc :: Account OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc{accOtherFields=cf}

$(makeLensesWith abbreviatedFields ''CustomField)
$(makeLensesWith abbreviatedFields ''Account)
-- $(makeLensesWith abbreviatedFields ''Contact)

createRequest :: (ToJSON cf, FromJSON cf) => OrgId -> Account cf -> Request
createRequest orgId acc = 
  let params = Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/chartofaccounts") params [] acc

create :: (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> Account cf -> m (Either Error (Account cf))
create orgId acc = 
  ZM.runRequestAndParseResponse $ createRequest orgId acc

data ListOpts = ListOpts
  { optPage :: !(Maybe Int)
  , optPerPage :: !(Maybe Int)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

listRequest :: OrgId -> ListOpts -> Request
listRequest orgId ListOpts{..} = 
  let params = ZO.applyOptionalQueryParam "per_page" (fmap show optPerPage) $
               ZO.applyOptionalQueryParam "page" (fmap show optPage) $
               Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint "/chartofaccounts") params []

list :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "chartofaccounts" [Account cf]))
list orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest orgId opts

-- fetchRequest :: OrgId -> ContactId -> Request
-- fetchRequest orgId conId = 
--   ZO.prepareGet (Common.mkApiEndpoint $ "/contacts/" <> toS conId) (Common.orgIdParam orgId) []

-- fetch :: (HasZoho m) => OrgId -> ContactId -> m (Either Error Contact)
-- fetch orgId conId = do
--   (ZM.runRequestAndParseResponse $ fetchRequest orgId conId) >>= \case
--     Left e -> pure $ Left e
--     Right (r :: ResponseWrapper "contact" Contact) -> pure $ Right $ unwrapResponse r


-- data ListOpts = ListOpts
--   { optPage :: !(Maybe Int)
--   , optPerPage :: !(Maybe Int)
--   } deriving (Eq, Show, Generic, EmptyZohoStructure)

-- listRequest :: OrgId -> ListOpts -> Request
-- listRequest orgId ListOpts{..} = 
--   let params = ZO.applyOptionalQueryParam "per_page" (fmap show optPerPage) $
--                ZO.applyOptionalQueryParam "page" (fmap show optPage) $
--                Common.orgIdParam orgId
--   in ZO.prepareGet (Common.mkApiEndpoint "/contacts") params []

-- list :: (HasZoho m) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "contacts" [Contact]))
-- list orgId opts = 
--   ZM.runRequestAndParseResponse $ listRequest orgId opts
