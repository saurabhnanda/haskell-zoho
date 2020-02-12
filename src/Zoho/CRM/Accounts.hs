{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Zoho.CRM.Accounts where

import Zoho.OAuth as ZO
import Network.OAuth.OAuth2 as O
import Network.Wreq as W hiding (Proxy(..))
import Zoho.CRM.Records as R
import Zoho.CRM.Common as Common
import Zoho.Types
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Data.Aeson.TH
import Data.ByteString as BS
import Network.HTTP.Client (Manager, ManagerSettings(..), newManager)
import Data.Text (Text)
import Data.Time
import qualified Data.Aeson.Types as Aeson
import Control.Lens
import GHC.Generics
import Control.Lens
import Data.Aeson.Types as Aeson (Parser)
import Zoho.ZohoM as ZM
import Data.List.NonEmpty as NE
import Data.Proxy
import Zoho.CRM.Common.Utils (googleAdsJsonOptions, pascalSnakeCase)
import Zoho.Types (zohoPrefix)

type AccountId = Text

data Account cf = Account
  { accountId :: Maybe AccountId
  , accountName :: Maybe Text
  , accountDescription :: Maybe Text
  , accountMetaData :: Maybe RecordMetaData
  , accountSpecialFields :: Maybe SpecialFields
  , accountOtherFields :: Maybe cf
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyAccount :: Account cf
emptyAccount = emptyZohoStructure

accountParser :: (Aeson.Value -> Parser (Maybe cf)) -> Aeson.Value -> Parser (Account cf)
accountParser otherParser v = do
  accountMetaData <- parseJSON v
  accountSpecialFields <- parseJSON v
  accountOtherFields <- otherParser v
  case v of
    Aeson.Object o -> do
      accountId <- o .:? "id"
      accountName <- o .:? "Account_Name"
      accountDescription <- o .:? "Description"
      pure Account{..}
    x -> fail "Expecting an Object to parse into a Contact"

-- instance {-# OVERLAPS #-} FromJSON (Contact ()) where
--   parseJSON = withObject "Exepcting a JSON object to parse into a Contact" $ \o -> do
--     let x = Object o
--     contactParser (const $ pure Nothing) x

instance (FromJSON cf) => FromJSON (Account cf) where
  parseJSON = accountParser parseJSON

instance (ToJSON cf) => ToJSON (Account cf) where
  toJSON Account{..} =
    unsafeMergeObjects (toJSON accountMetaData) $
    unsafeMergeObjects (toJSON accountSpecialFields) $
    unsafeMergeObjects (toJSON accountOtherFields) $
    object $
      omitNothing "id" accountId <>
      omitNothing "Account_Name" accountName <>
      omitNothing "Description" accountDescription
    where
      omitNothing k v = case v of
        Nothing -> []
        Just x -> [k Aeson..= x]

$(makeLensesWith abbreviatedFields ''Account)
