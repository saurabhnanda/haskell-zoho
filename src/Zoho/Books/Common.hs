module Zoho.Books.Common where

import qualified Data.ByteString as BS
import Zoho.Types
import Data.String.Conv
import qualified Zoho.OAuth as ZO
import URI.ByteString as U
import Data.Text (Text)
import Data.Aeson as Aeson
import GHC.Generics
import Control.Lens
import Data.Aeson.Casing as Casing
import Network.HTTP.Types as HT
import qualified Data.ByteString as BS
import Zoho.CRM.Common (ZohoResult(..))

mkApiEndpoint :: BS.ByteString -> URI
mkApiEndpoint p = ZO.mkEndpoint (Host "www.zohoapis.com") ("/books/v3" <> p)

orgIdParam :: OrgId -> [(BS.ByteString, Maybe BS.ByteString)]
orgIdParam (OrgId oid) = [("organization_id", Just $ toS oid)]

data CustomField = CustomField
  { cfCustomfieldId :: !Text
  , cfValue :: !Aeson.Value
  } deriving (Eq, Show, Generic)

$(makeLensesWith abbreviatedFields ''CustomField)

instance ToJSON CustomField where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON CustomField where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

newtype TaxId = TaxId {rawTaxId :: Text}
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text
  deriving (FromJSON) via Text


newtype CurrencyId = CurrencyId {rawCurrencyId :: Text}
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text
  deriving (FromJSON) via Text

newtype TdsId = TdsId {rawTdsId :: Text}
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text
  deriving (FromJSON) via Text

type DeleteResult = ZohoResult OmitField OmitField
type OperationResult = ZohoResult OmitField OmitField

data ListOp = OpStartsWith !Text | OpContains !Text deriving  (Eq, Show, Generic)

data UnsafeEither a b = UnsafeLeft !a | UnsafeRight !b deriving (Eq, Show, Generic)

instance (ToJSON a, ToJSON b) => ToJSON (UnsafeEither a b) where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase){sumEncoding=UntaggedValue} 

instance (FromJSON a, FromJSON b) => FromJSON (UnsafeEither a b) where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase){sumEncoding=UntaggedValue} 

applyOptionalListOp :: BS.ByteString -> Maybe ListOp -> HT.Query -> HT.Query
applyOptionalListOp k mListOp qp = 
  case mListOp of
    Nothing -> qp
    Just (OpStartsWith x) ->(k <> "_startswith", Just $ toS x):qp
    Just (OpContains x) ->(k <> "_contains", Just $ toS x):qp

class HasOtherFields s a | s -> a where
  otherFields :: Lens' s a