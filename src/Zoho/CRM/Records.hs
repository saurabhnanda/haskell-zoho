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

list :: (FromJSON a)
     => BS.ByteString
     -> ListOptions
     -> Manager
     -> AccessToken
     -> IO (W.Response (Either String (PaginatedResponse "data" a)))
list modApiName listopts mgr tkn = do
  r <- ZO.authGetJSON (qparams listopts) (apiEndpointStr modApiName) mgr tkn
  pure $ fmap eitherDecode r
  -- pure r
  where
    applyOptionalParam k mVal opt = case mVal of
      Nothing -> opt
      Just val -> opt & (param k) .~ [val]
    applyOptionalHeader h mVal opt = case mVal of
      Nothing -> opt
      Just val -> opt & (header h) .~ [val]
    iso8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
    qparams ListOptions{..} =
      applyOptionalHeader "If-Modified-Since" (fmap (toS . iso8601) optModifiedAfter) $
      applyOptionalParam "fields" (fmap (T.intercalate ",") optFields) $
      applyOptionalParam "per_page" (fmap (toS . show) optPerPage) $
      W.defaults

getSpecificRecord :: forall a . (FromJSON a)
                  => BS.ByteString
                  -> Text
                  -> Manager
                  -> AccessToken
                  -> IO (W.Response (Either String (Maybe a)))
getSpecificRecord modApiName recordId mgr tkn = do
  r <- ZO.authGetJSON W.defaults (apiEndpointStr modApiName <> "/" <> toS recordId) mgr tkn
  pure $ fmap parseResonse r
  where
    parseResonse r =
      if r == mempty
      then Right Nothing
      else case (eitherDecode r :: Either String (ResponseWrapper "data" [a])) of
        Left e -> Left e
        Right x -> Right $ listToMaybe $ unwrapResponse x

insert :: forall a . (ToJSON a)
       => BS.ByteString
       -> [a]
       -> Manager
       -> AccessToken
       -> IO (W.Response (Either String [Aeson.Value]))
insert modApiName records mgr tkn = do
  r <- ZO.authPost W.defaults (apiEndpointStr modApiName) (toJSON pload) mgr tkn
  pure $ fmap parseResponse r
  where
    pload :: ResponseWrapper "data" [a]
    pload = ResponseWrapper records

    parseResponse bsl =
      case eitherDecode bsl :: Either String (ResponseWrapper "data" [Aeson.Value]) of
        Left e -> Left e
        Right r -> Right $ unwrapResponse r
