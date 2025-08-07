module Zoho.Desk.Common
  ( module Zoho.Desk.Common
  , module Zoho.Types
  )
where

import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.ByteString as BS
import Network.HTTP.Types (Header)
import Zoho.Types (OrgId(..), ApiName)
import URI.ByteString as U
import Zoho.OAuth as ZO
import Data.String.Conv (toS)
import Zoho.Types
import Data.Aeson.Casing as Casing
import Prelude hiding (id)
import Control.Lens hiding (from)
import Data.Text (Text)
import Data.Time
import Network.HTTP.Types as HT(Query)
import Data.List as DL
import Prelude hiding (id)

type TicketId = Text
type ThreadId = Text
type DepartmentId = Text
type ContactId = Text
type ProductId = Text
type AccountId = Text
type AssigneeId = Text
type TeamId = Text
type ViewId = Text
type ArticleId = Text
type CategoryId = Text
type AuthorId = Text
type TranslationId = Text


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
-- $(makeLenses ''SearchResults)


data SortDirection = SortAsc | SortDesc deriving (Eq, Show, Enum)

class HasFrom s a | s -> a where from :: Lens' s a
class HasLimit s a | s -> a where limit :: Lens' s a
-- class HasId s a | s -> a where id :: Lens' s a
class HasAccountName s a | s -> a where accountName :: Lens' s a
class HasAll s a | s -> a where all :: Lens' s a
class HasCustomFields s a | s -> a where customFields :: Lens' s a
-- class HasCustomField2 s a | s -> a where customField2 :: Lens' s a
-- class HasCustomField3 s a | s -> a where customField3 :: Lens' s a
-- class HasCustomField4 s a | s -> a where customField4 :: Lens' s a
-- class HasCustomField5 s a | s -> a where customField5 :: Lens' s a
-- class HasCustomField6 s a | s -> a where customField6 :: Lens' s a
-- class HasCustomField7 s a | s -> a where customField7 :: Lens' s a
-- class HasCustomField8 s a | s -> a where customField8 :: Lens' s a
-- class HasCustomField9 s a | s -> a where customField9 :: Lens' s a
-- class HasCustomField10 s a | s -> a where customField10 :: Lens' s a
class HasCreatedTimeRange s a | s -> a where createdTimeRange :: Lens' s a
class HasModifiedTimeRange s a | s -> a where modifiedTimeRange :: Lens' s a

applyCommonSearchParams :: ( HasFrom opts (Maybe Int)
                           , HasLimit opts (Maybe Int)
                           , HasId opts (Maybe Text)
                           , HasAll opts (Maybe Bool)
                           , HasCreatedTimeRange opts (Maybe (UTCTime, UTCTime))
                           , HasModifiedTimeRange opts (Maybe (UTCTime, UTCTime))
                           )
                        => opts
                        -> HT.Query
                        -> HT.Query
applyCommonSearchParams opts p =
  applyTimeRangeParam "modifiedTimeRange" (opts ^. modifiedTimeRange) $
  applyTimeRangeParam "createdTimeRange" (opts ^. createdTimeRange) $
  applyOptionalQueryParam "id" (opts ^. id) $
  applyOptionalQueryParam "limit" (show <$> opts ^. limit) $
  applyOptionalQueryParam "from" (show <$> opts ^. from)
  p


applyTimeRangeParam :: BS.ByteString
                    -> Maybe (UTCTime, UTCTime)
                    -> HT.Query
                    -> HT.Query
applyTimeRangeParam k Nothing p = p
applyTimeRangeParam k (Just (t1, t2)) p =
  (k, Just $ toS $ (iso8601 t1) <> ":" <> (iso8601 t2)):p
  where
    iso8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"


applyCustomFieldSearchParams :: (HasCustomFields opts [(ApiName, Text)])
                             => opts
                             -> HT.Query
                             -> HT.Query
applyCustomFieldSearchParams opts p =
  DL.foldl' fn p $ Prelude.zip (opts ^. customFields) [1..]
  where
    fn memo ((cfName, cfVal), idx) =
      let x = ( toS $ "customField" <> show idx
              , Just $ toS $ cfName <> ":" <> cfVal )
      in x:memo
