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
  }

list :: BS.ByteString
     -> ListOptions
     -> Manager
     -> AccessToken
--     -> IO (W.Response (Either String (PaginatedResponse "data" BS.ByteString)))
     -> IO (W.Response (Either String (PaginatedResponse "data" [VisitSummary])))
list modApiName listopts mgr tkn = do
  r <- ZO.authGetJSON (qparams listopts) (apiEndpointStr modApiName) mgr tkn
  pure $ fmap eitherDecode r
  -- pure r
  where
    applyOptional k mVal opt = case mVal of
      Nothing -> opt
      Just val -> opt & (param k) .~ [val]
    qparams ListOptions{..} =
      applyOptional "fields" (fmap (T.intercalate ",") optFields) $
      applyOptional "per_page" (fmap (toS . show) optPerPage) $
      W.defaults

