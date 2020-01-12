module Zoho.CRM.Common where

import Zoho.OAuth as ZO
import Network.OAuth.OAuth2 as O
import Network.Wreq as W hiding (Proxy(..))
import Zoho.Types
import Data.Time
import Data.Aeson as Aeson
import Data.Aeson.TH
import Data.Aeson.Casing as Casing
import Data.Text as Text
import Zoho.CRM.Common.Utils (googleAdsJsonOptions, pascalSnakeCase)
import Zoho.Types (zohoPrefix)
import Control.Lens
import GHC.Generics
import Data.Aeson.Types as Aeson (Parser)
import Debug.Trace
import Control.Applicative ((<|>))

data VisitSummary = VisitSummary
  { vsFirstVisitedTime :: Maybe ZonedTime
  , vsFirstVisitedURL :: Maybe Text
  , vsReferrer :: Maybe Text
  , vsLastVisitedTime :: Maybe ZonedTime
  , vsNumberOfChats :: Maybe Int
  , vsVisitorScore :: Maybe Text
  , vsAverageTimeSpentMinutes :: Maybe Float
  , vsDaysVisited :: Maybe Int
  } deriving (Show)

instance EmptyZohoStructure VisitSummary where
  emptyZohoStructure = emptyVisitSummary

emptyVisitSummary :: VisitSummary
emptyVisitSummary = VisitSummary
  { vsFirstVisitedTime = Nothing
  , vsFirstVisitedURL = Nothing
  , vsReferrer = Nothing
  , vsLastVisitedTime = Nothing
  , vsNumberOfChats = Nothing
  , vsVisitorScore = Nothing
  , vsAverageTimeSpentMinutes = Nothing
  , vsDaysVisited = Nothing
  }


data ScoreSummary = ScoreSummary
  { ssScore :: Maybe Int
  , ssPositiveScore :: Maybe Int
  , ssNegativeScore :: Maybe Int
  , ssTouchPointScore :: Maybe Int
  , ssPositiveTouchPointScore :: Maybe Int
  , ssNegativeTouchPointScore :: Maybe Int
  } deriving (Eq, Show)

emptyScoreSummary :: ScoreSummary
emptyScoreSummary = ScoreSummary
  { ssScore = Nothing
  , ssPositiveScore = Nothing
  , ssNegativeScore = Nothing
  , ssTouchPointScore = Nothing
  , ssPositiveTouchPointScore = Nothing
  , ssNegativeTouchPointScore = Nothing
  }

instance EmptyZohoStructure ScoreSummary where
  emptyZohoStructure = emptyScoreSummary

data GoogleAdsInfo = GoogleAdsInfo
  { gadsGCLID :: Maybe Text
  , gadsClickType :: Maybe Text
  , gadsAdNetwork :: Maybe Text
  , gadsAdCampaignName :: Maybe Text
  , gadsAd :: Maybe Text
  , gadsAdGroupName :: Maybe Text
  , gadsClickDate :: Maybe Text -- TODO: This type needs to change!
  , gadsCostPerClick :: Maybe Float
  , gadsCostPerConversion :: Maybe Float
  , gadsConversionExportStatus :: Maybe Text
  , gadsConversionExportedOn :: Maybe Text -- TODO : change this type
  , gadsReasonForConversionFailure :: Maybe Text
  , gadsKeyword :: Maybe Text
  , gadsDeviceType :: Maybe Text
  , gadsSearchPartnerNetwork :: Maybe Text
  } deriving (Eq, Show)

emptyGoogleAdsInfo :: GoogleAdsInfo
emptyGoogleAdsInfo = GoogleAdsInfo
  { gadsGCLID = Nothing
  , gadsClickType = Nothing
  , gadsAdNetwork = Nothing
  , gadsAdCampaignName = Nothing
  , gadsAd = Nothing
  , gadsAdGroupName = Nothing
  , gadsClickDate = Nothing
  , gadsCostPerClick = Nothing
  , gadsCostPerConversion = Nothing
  , gadsConversionExportStatus = Nothing
  , gadsConversionExportedOn = Nothing
  , gadsReasonForConversionFailure = Nothing
  , gadsKeyword = Nothing
  , gadsDeviceType = Nothing
  , gadsSearchPartnerNetwork = Nothing
  }

instance EmptyZohoStructure GoogleAdsInfo where
  emptyZohoStructure = emptyGoogleAdsInfo


data RecordMetaData = RecordMetaData
  { rmdCreatedTime :: Maybe ZonedTime
  , rmdModifiedTime :: Maybe ZonedTime
  , rmdCreatedBy :: Maybe (Reference "name")
  , rmdModifiedBy :: Maybe (Reference "name")
  , rmdOwner :: Maybe (Reference "name")
  , rmdLastActivityTime :: Maybe ZonedTime
  , rmdTag :: Maybe [Reference "name"]
  } deriving (Show)

emptyRecordMetaData :: RecordMetaData
emptyRecordMetaData = RecordMetaData
  { rmdCreatedTime = Nothing
  , rmdModifiedTime = Nothing
  , rmdCreatedBy = Nothing
  , rmdModifiedBy = Nothing
  , rmdOwner = Nothing
  , rmdLastActivityTime = Nothing
  , rmdTag = Nothing
  }

instance EmptyZohoStructure RecordMetaData where
  emptyZohoStructure = emptyRecordMetaData

data OnlyMetaData = OnlyMetaData
  { omdCreatedTime :: ZonedTime
  , omdModifiedTime :: ZonedTime
  , omdCreatedBy :: Reference "name"
  , omdModifiedBy :: Reference "name"
  } deriving (Show)

data OnlyId = OnlyId { unwrapId :: !Text } deriving (Eq, Show, Generic)
instance ToJSON OnlyId where
  toJSON (OnlyId i) = object [ "id" Aeson..= i ]

instance FromJSON OnlyId where
  parseJSON = withObject "Expecting object to parse into OnlyId record" $ \o -> do
    unwrapId <- o .: "id"
    pure OnlyId{..}

data ZohoCode = ZCodeInvalidIToken
              | ZCodeSuccess
              | ZCodeOther !Text
              deriving (Eq, Show, Generic, Ord)

instance FromJSON ZohoCode where
  parseJSON = withText "Expecting text to parse into ZohoCode" $ \t -> pure $ case t of
    "INVALID_TOKEN" -> ZCodeInvalidIToken
    "INVALID_OAUTH" -> ZCodeInvalidIToken
    "SUCCESS" -> ZCodeSuccess
    _ -> ZCodeOther t

data ZohoStatus = ZStatusError
                | ZStatusSuccess
                | ZStatusOther !Text
                deriving (Eq, Show, Generic, Ord)

instance FromJSON ZohoStatus where
  parseJSON = withText "Expecting text to parse into ZohoStatus" $ \t -> pure $ case t of
    "error" -> ZStatusError
    "success" -> ZStatusSuccess
    _ -> ZStatusOther t

data UpsertAction = UAInsert
                  | UAUpdate !ApiName
                  | UAOther !(Maybe Text) !(Maybe ApiName)
                  deriving (Eq, Show)

instance FromJSON UpsertAction where
  parseJSON v = (flip (withObject "Expecting Object to parse into an UpserResult")) v $ \o -> do
    ma <- o .:? "action"
    mf <- o .:? "duplicate_field"
    case traceShowId (ma, mf) of
      (Just "insert", _) -> pure UAInsert
      (Just "update", Just f) -> pure $ UAUpdate f
      _ -> pure $ UAOther ma mf

data ZohoResult details upsert = ZohoResult
  { zresCode :: !ZohoCode
  , zresDetails :: !details
  , zresMessage :: !Text
  , zresStatus :: !ZohoStatus
  , zresUpsertAction :: !upsert
  } deriving (Eq, Show, Generic)

type InsertResult = ZohoResult OnlyMetaData ()
type UpdateResult = ZohoResult OnlyMetaData ()
type UpsertResult = ZohoResult OnlyMetaData UpsertAction
type DeleteResult = ZohoResult OnlyId ()

instance {-# OVERLAPS #-} FromJSON (ZohoResult () ()) where
  parseJSON = zohoResultParser (const $ pure ()) (const $ pure ())

instance {-# OVERLAPS #-} (FromJSON details) => FromJSON (ZohoResult details ()) where
  parseJSON = zohoResultParser parseJSON (const $ pure ())

instance (FromJSON details, FromJSON upsert) => FromJSON (ZohoResult details upsert) where
  parseJSON = zohoResultParser parseJSON parseJSON

zohoResultParser :: (FromJSON details)
                 => (Aeson.Value -> Parser details)
                 -> (Aeson.Value -> Parser upsert)
                 -> Aeson.Value
                 -> Parser (ZohoResult details upsert)
zohoResultParser dparser uparser v =
  (flip (withObject "Expecting Object to parse into an UpserResult")) v $ \o -> do
  zresCode <- (o .: "code") <|> (o .: "errorCode")
  zresMessage <- o .: "message"
  zresStatus <- (o .: "status") <|> (pure ZStatusError)
  zresDetails <- dparser v
  zresUpsertAction <- uparser v
  pure ZohoResult{..}

$(deriveJSON (zohoPrefix pascalSnakeCase) ''VisitSummary)
$(deriveJSON (zohoPrefix pascalSnakeCase) ''ScoreSummary)
$(deriveJSON (zohoPrefix pascalSnakeCase) ''RecordMetaData)
$(deriveJSON googleAdsJsonOptions ''GoogleAdsInfo)
$(deriveJSON (zohoPrefix pascalSnakeCase) ''OnlyMetaData)
$(makeLensesWith abbreviatedFields ''VisitSummary)
$(makeLensesWith abbreviatedFields ''ScoreSummary)
$(makeLensesWith abbreviatedFields ''RecordMetaData)
$(makeLensesWith abbreviatedFields ''GoogleAdsInfo)
$(makeLensesWith abbreviatedFields ''OnlyMetaData)
