module Zoho.CRM.Records where

import Zoho.OAuth as ZO
import Network.OAuth.OAuth2 as O
import Network.Wreq as W hiding (Proxy(..))
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Network.Wreq as W
import Data.Aeson as Aeson
import Data.String.Conv (toS)
import Network.HTTP.Client (Manager)
import URI.ByteString as U
import Zoho.Types
import qualified Data.Text as T
import Data.Text (Text)
import Control.Lens
import Zoho.CRM.Common
import Data.Time
import Data.Maybe (listToMaybe)
import Zoho.ZohoM as ZM
import Network.HTTP.Client
import Network.HTTP.Types

apiEndpoint :: BS.ByteString -> URI
apiEndpoint modApiName  = ZO.mkApiEndpoint $ "/crm/v2/" <> modApiName

apiEndpointStr :: BS.ByteString -> String
apiEndpointStr modApiName = toS $ serializeURIRef' $ apiEndpoint modApiName


data SortOrder = Asc | Desc deriving (Eq, Show, Ord, Enum)
data TriState = TriTrue | TriFalse | TriBoth deriving (Eq, Show, Ord, Enum)

type ApiName = Text

data ListOptions = ListOptions
  { optFields :: Maybe [ApiName]
  , optSortOrder :: Maybe SortOrder
  , optSortBy :: Maybe ApiName
  , optConverted :: Maybe TriState
  , optApproved :: Maybe TriState
  , optPage :: Maybe Int
  , optPerPage :: Maybe Int
  , optCustomViewId :: Maybe Int
  , optTerritory :: Maybe (Int, Bool)
  , optModifiedAfter :: Maybe UTCTime
  } deriving (Eq, Show)

-- TODO: Make emptyZohoStructure out of this.
defaultListOptions :: ListOptions
defaultListOptions = ListOptions
  { optFields = Nothing
  , optSortOrder = Nothing
  , optSortBy = Nothing
  , optConverted = Nothing
  , optApproved = Nothing
  , optPage = Nothing
  , optPerPage = Nothing
  , optCustomViewId = Nothing
  , optTerritory = Nothing
  , optModifiedAfter = Nothing
  }


list :: (FromJSON a, HasZoho m)
     => BS.ByteString
     -> ListOptions
     -> m (Either Error (PaginatedResponse "data" a))
list modApiName listopts = ZM.runRequestAndParseResponse (listRequest modApiName listopts)


listRequest :: BS.ByteString
            -> ListOptions
            -> Request
listRequest modApiName ListOptions{..} =
  ZO.prepareGet (apiEndpoint modApiName) qparams headers
  where
    applyOptionalParam k mVal opt = case mVal of
      Nothing -> opt
      Just val -> (k, Just $ toS val):opt

    applyOptionalHeader h mVal hs = case mVal of
      Nothing -> hs
      Just val -> (h, toS val):hs

    iso8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"

    headers =
      applyOptionalHeader hIfModifiedSince (fmap iso8601 optModifiedAfter)
      []

    qparams =
      applyOptionalParam "fields" (fmap (T.intercalate ",") optFields) $
      applyOptionalParam "per_page" (fmap show optPerPage)
      []

-- getSpecificRecord :: forall a . (FromJSON a)
--                   => BS.ByteString
--                   -> Text
--                   -> Manager
--                   -> AccessToken
--                   -> IO (W.Response (Either String (Maybe a)))
-- getSpecificRecord modApiName recordId mgr tkn = do
--   r <- ZO.authGetJSON W.defaults (apiEndpointStr modApiName <> "/" <> toS recordId) mgr tkn
--   pure $ fmap parseResonse r
--   where
--     parseResonse r =
--       if r == mempty
--       then Right Nothing
--       else case (eitherDecode r :: Either String (ResponseWrapper "data" [a])) of
--         Left e -> Left e
--         Right x -> Right $ listToMaybe $ unwrapResponse x

-- insert :: forall a . (ToJSON a)
--        => BS.ByteString
--        -> [a]
--        -> Manager
--        -> AccessToken
--        -> IO (W.Response (Either String [Aeson.Value]))
-- insert modApiName records mgr tkn = do
--   r <- ZO.authPost W.defaults (apiEndpointStr modApiName) (toJSON pload) mgr tkn
--   pure $ fmap parseResponse r
--   where
--     pload :: ResponseWrapper "data" [a]
--     pload = ResponseWrapper records

--     parseResponse bsl =
--       case eitherDecode bsl :: Either String (ResponseWrapper "data" [Aeson.Value]) of
--         Left e -> Left e
--         Right r -> Right $ unwrapResponse r
