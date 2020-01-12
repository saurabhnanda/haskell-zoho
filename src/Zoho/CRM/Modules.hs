{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zoho.CRM.Modules where

import Zoho.OAuth as ZO
import Network.OAuth.OAuth2 as O
import Network.Wreq as W hiding (Proxy(..))
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import URI.ByteString as U
import URI.ByteString.QQ
import Data.String.Conv (toS)
import Data.Aeson as Aeson
import Zoho.Types
import Data.Time
import Data.Aeson.Casing
import Data.Aeson.TH
import GHC.TypeLits
import Data.Proxy as P
import Data.String.Conv
import Data.Text
import Network.HTTP.Client (Manager)

data Module = Module
  { modGlobalSearchSupported :: Bool
  , modDeletable :: Bool
  , modCreatable :: Bool
  , modInventoryTemplateSupported :: Bool
  , modModifiedTime :: Maybe UTCTime
  , modPluralLabel :: Text
  , modPresenceSubMenu :: Bool
  , modId :: Text
  , modVisibility :: Int
  , modConvertable :: Bool
  , modEditable :: Bool
  , modEmailTemplateSupport :: Bool
  , modProfiles :: [Reference "name"]
  , modFilterSupported :: Bool
  , modShowAsTab :: Bool
  , modWebLink :: Maybe Text
  , modSequenceNumber :: Int
  , modSingularLabel :: Text
  , modViewable :: Bool
  , modApiSupported :: Bool
  , modApiName :: Text
  , modQuickCreate :: Bool
  , modModifiedBy :: Maybe (Reference "name")
  , modGeneratedType :: Text -- TODO
  , modFeedsRequired :: Bool
  , modScoringSupported :: Bool
  , modWebformSupported :: Bool
  , modArguments :: ()
  , modModuleName :: Text
  , modBusinessCardFieldLimit :: Int
  , modParentModule :: Maybe (Reference "api_name")
  } deriving (Eq, Show)


$(deriveJSON (aesonPrefix moduleJsonFieldNameMapping) ''Module)

apiEndpoint :: URI
apiEndpoint = ZO.mkApiEndpoint "/crm/v2/settings/modules"

apiEndpointStr :: String
apiEndpointStr = toS $ serializeURIRef' apiEndpoint

-- list :: Manager
--      -> AccessToken
--      -> IO (W.Response (Either String (ResponseWrapper "modules" [Module])))
-- list mgr tkn = do
--   r <- ZO.authGetJSON W.defaults apiEndpointStr mgr tkn
--   pure $ fmap eitherDecode r


