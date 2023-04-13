{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Zoho.Books.Item where

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
import Zoho.Books.Common (CustomField(..))
import Network.HTTP.Client as HC (Request)
import qualified Data.HashMap.Lazy as HML
import Zoho.Books.Account (AccountId(..))
import Zoho.Books.Common (TaxId(..))
-- import Data.String.Conv

newtype ItemId = ItemId { rawItemId :: Text } 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text
  deriving ToJSONKey via Text
  deriving FromJSONKey via Text

data Item cf = Item
  { itemItemId :: !(Maybe ItemId)
  , itemName :: !(Maybe Text)
  , itemDescription :: !(Maybe Text)
  , itemRate :: !(Maybe Double)
  , itemTaxId :: !(Maybe TaxId)
  , itemSku :: !(Maybe Text)
  , itemProductType :: !(Maybe Text)
  , itemHsnOrSac :: !(Maybe Text)
  , itemIsTaxable :: !(Maybe Bool)
  , itemAccountId :: !(Maybe AccountId)
  , itemItemType :: !(Maybe Text)
  , itemCustomFields :: !(Maybe [Common.CustomField])
  , itemCreatedTime :: !(Maybe UTCTime)
  , itemLastModifiedTime :: !(Maybe UTCTime)
  , itemOtherFields :: !(Maybe cf)
  } deriving (Eq, Show, Generic)

instance (ToJSON cf) => ToJSON (Item cf) where
  toJSON :: ToJSON cf => Item cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{itemOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (itemOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ HML.delete "other_fields" o
      _ -> y

instance (FromJSON cf) => FromJSON (Item cf) where
  parseJSON v = do
    acc :: Item OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc{itemOtherFields=cf}

$(makeLensesWith abbreviatedFields ''Item)
-- $(makeLensesWith abbreviatedFields ''Contact)

createRequest :: (ToJSON cf, FromJSON cf) => OrgId -> Item cf -> Request
createRequest orgId acc = 
  let params = Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/items") params [] acc

create :: (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> Item cf -> m (Either Error (Item cf))
create orgId acc = do
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
  in ZO.prepareGet (Common.mkApiEndpoint "/items") params []

list :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "items" [Item cf]))
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
