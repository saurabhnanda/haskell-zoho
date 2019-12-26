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
import Network.HTTP.Client (Manager, ManagerSettings(..), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson as Aeson
import Zoho.Types
import Data.Time
import Data.Aeson.Casing
import Data.Aeson.TH
import GHC.TypeLits
import Data.Proxy as P
import Data.String.Conv
import Data.Text
import Debug.Trace

data Reference (s :: Symbol) = Reference
  { refId :: Text
  , refName :: Text
  } deriving (Eq, Show)

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

instance {-# OVERLAPS #-} (KnownSymbol s) => FromJSON (Maybe (Reference s)) where
  parseJSON v = case v of
    Aeson.Null -> pure Nothing
    Aeson.Object o -> if o == mempty
                      then pure Nothing
                      else fmap Just $ parseJSON v
    _ -> fmap Just $ parseJSON v

instance {-# OVERLAPS #-} (KnownSymbol s) => ToJSON (Maybe (Reference s)) where
  toJSON mRef = case mRef of
    Nothing -> object []
    Just ref -> toJSON ref

instance (KnownSymbol s) => FromJSON (Reference s) where
  parseJSON = withObject "Expecting Object to parse into a Reference" $ \o -> do
    refId <- o .: "id"
    refName <- o .: (toS $ symbolVal (Proxy :: Proxy s))
    pure Reference{..}


instance (KnownSymbol s) => ToJSON (Reference s) where
  toJSON Reference{..} = object
    [ "id" .= refId
    , (toS $ symbolVal (Proxy :: Proxy s)) .= refName
    ]

$(deriveJSON (aesonPrefix moduleJsonFieldNameMapping) ''Module)

-- instance FromJSON Module where
--   parseJSON = withObject "Expected Object to parse into Module" $ \o -> do

apiEndpoint :: URI
apiEndpoint = ZO.mkApiEndpoint "/crm/v2/settings/modules"

apiEndpointStr :: String
apiEndpointStr = toS $ serializeURIRef' apiEndpoint

list :: Manager
     -> AccessToken
     -> IO (W.Response (Either String (ResponseWrapper "modules" [Module])))
list mgr tkn = do
  r <- ZO.authGetJSON W.defaults apiEndpointStr mgr tkn
  pure $ fmap eitherDecode r



zohoOAuth :: OAuth2
zohoOAuth = mkOAuth hostUS (ClientId "1000.PCRP10N4ZKXC7F029BTTP6UT594BIH") (ClientSecret "67d211c3cb5c31df1a1899462514fba3abe152f6cb") ([uri|http://master.hetzner.vacationlabs.com/lambda/oauth-redirect|])

zohoManager :: IO Manager
zohoManager = newManager $ tlsManagerSettings{managerModifyRequest=logRequest}

test = do
  let rtkn = RefreshToken "1000.7950f276ab5889010ba61d5074835d16.84a6e76f73e09303f32e408c5ccb298f"
  mgr <- zohoManager
  withAccessToken mgr zohoOAuth rtkn Nothing list
  -- refreshAccessToken mgr oa rtkn

test2 = do
  BS.putStrLn $ serializeURIRef' $ ZO.authorizationUrl [Scope "ZohoCRM.modules.ALL", Scope "ZohoCRM.settings.ALL"] zohoOAuth
  BS.putStrLn "Please enter exchange token:"
  t <- Prelude.getLine
  mgr <- zohoManager
  O.fetchAccessToken2 mgr zohoOAuth (ExchangeToken $ toS t)
