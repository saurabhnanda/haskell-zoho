module Zoho.Desk.Common where

import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.ByteString as BS
import Network.HTTP.Types (Header)
import Zoho.Types (OrgId(..))
import URI.ByteString as U
import Zoho.OAuth as ZO
import Data.String.Conv (toS)
import Zoho.Types (zohoPrefix)
import Data.Aeson.Casing as Casing

-- data ErrorCode = ZInvalidToken
--                | ZCodeOther !Text
--                 deriving (Eq, Sho, Ord)

-- instance FromJSON ErrorCode where
--   parseJSON = withText "Expecting Text to parse into Zoho.Desk.Common.ErrorCode" $ \t -> pure $ case t of
--     "INVALID_OAUTH" -> ZInvalidToken
--     _ -> ZCodeOther t

-- data ZohoResult = ZohoResult
--   { resErrorCode :: !ErrorCode
--   , resMessage :: !(Maybe Text)
--   } deriving (Eq, Show)

-- instance FromJSON ZohoResult where
--   parseJSON = withObject "Expecting Object to parse into Zoho.Desk.Common.ZohoResult" $ \o -> do
--     resErrorCode <- o .: "errorCode"
--     resMessage <- o .: "message"
--     pure ZohoResult{..}


mkApiEndpoint :: BS.ByteString -> URI
mkApiEndpoint p = ZO.mkEndpoint (Host "desk.zoho.com") ("/api/v1" <> p)

orgIdHeader :: OrgId -> Header
orgIdHeader (OrgId oid) = ("orgId", toS oid)

data SearchResults a = SearchResults
  { searchData :: [a]
  , searchCount :: !Int
  } deriving (Eq, Show)

$(Aeson.deriveJSON (zohoPrefix Casing.camelCase) ''SearchResults)

