{-# LANGUAGE DeriveAnyClass #-}
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
import Zoho.CRM.Common.Utils (googleAdsJsonOptions)
import Zoho.Types (zohoPrefix, pascalSnakeCase)
import Control.Lens
import GHC.Generics
import Data.Aeson.Types as Aeson (Parser)
import Debug.Trace
import Control.Applicative ((<|>))

data Approval = Approval
  { apDelegate :: Maybe Bool -- delegate
  , apApprove :: Maybe Bool
  , apReject :: Maybe Bool
  , apResubmit :: Maybe Bool
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyApproval :: Approval
emptyApproval = emptyZohoStructure


data SpecialFields = SpecialFields
  { csfCurrencySymbol :: Maybe Text -- $currency_symbol
  , csfState :: Maybe Text -- $state
  , csfProcessFlow :: Maybe Bool -- $process_flow
  , csfApproved :: Maybe Bool -- $approved
  , csfApproval :: Maybe Approval  -- $approval
  , csfEditable :: Maybe Bool -- $editable

  -- TODO: Figure out what is "review" all about
  -- , csfReviewProcess :: Maybe _ -- $review_process
  -- , csvReview :: Maybe _ -- $review
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptySpecialFields :: SpecialFields
emptySpecialFields = emptyZohoStructure

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
  { gadsGclid :: Maybe Text
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
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyGoogleAdsInfo :: GoogleAdsInfo
emptyGoogleAdsInfo = emptyZohoStructure

data MetaData = MetaData
  { rmdCreatedTime :: Maybe ZonedTime
  , rmdModifiedTime :: Maybe ZonedTime
  , rmdCreatedBy :: Maybe (Reference "name")
  , rmdModifiedBy :: Maybe (Reference "name")
  , rmdOwner :: Maybe (Reference "name")
  } deriving (Show, Generic, EmptyZohoStructure)

instance Eq MetaData where
  (==) a b = (rmdCreatedBy a) == (rmdCreatedBy b) &&
             (rmdModifiedBy a) == (rmdModifiedBy b) &&
             (rmdOwner a) == (rmdOwner b) &&
             (fmap zonedTimeToUTC $ rmdCreatedTime a) == (fmap zonedTimeToUTC $ rmdCreatedTime b) &&
             (fmap zonedTimeToUTC $ rmdModifiedTime a) == (fmap zonedTimeToUTC $ rmdModifiedTime b)

emptyMetaData :: MetaData
emptyMetaData = emptyZohoStructure

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

data ZohoCode = ZCodeInvalidToken
              | ZCodeSuccess
              | ZCodeInvalidData
              | ZCodeOther !Text
              deriving (Eq, Show, Generic, Ord)

instance FromJSON ZohoCode where
  parseJSON = withText "Expecting text to parse into ZohoCode" $ \t -> pure $ case t of
    "INVALID_TOKEN" -> ZCodeInvalidToken
    "INVALID_OAUTH" -> ZCodeInvalidToken
    "INVALID_DATA" -> ZCodeInvalidData
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
  parseJSON v = (flip (withObject "Expecting Object to parse into an UpsertAction")) v $ \o -> do
    ma <- o .:? "action"
    mf <- o .:? "duplicate_field"
    case (ma, mf) of
      (Just "insert", _) -> pure UAInsert
      (Just "update", Just f) -> pure $ UAUpdate f
      _ -> pure $ UAOther ma mf

data ZohoResult metadata action = ZohoResult
  { zresCode :: !ZohoCode
  , zresMetaData :: !(Maybe metadata)
  , zresDetails :: !Aeson.Value
  , zresMessage :: !Text
  , zresStatus :: !ZohoStatus
  , zresAction :: !(Maybe action)
  } deriving (Eq, Show, Generic)

type InsertResult = ZohoResult OnlyMetaData OmitField
type UpdateResult = ZohoResult OnlyMetaData OmitField
type UpsertResult = ZohoResult OnlyMetaData UpsertAction
type DeleteResult = ZohoResult OnlyId OmitField

instance (FromJSON metadata, FromJSON action) => FromJSON (ZohoResult metadata action) where
  parseJSON = withObject "Expecting Object to parse into ZohoResult" $ \o -> do
    zresCode <- (o .: "code") <|> (o .: "errorCode")
    zresMetaData <- (o .: "details") <|> (pure Nothing)
    zresDetails <- (o .:? "details") >>= \case
      Nothing -> pure Aeson.Null
      Just x -> pure x
    zresMessage <- o .: "message"
    zresStatus <- (o .: "status") <|> (pure ZStatusError)
    zresAction <- (parseJSON (Aeson.Object o)) <|> (pure Nothing)
    pure ZohoResult{..}

-- instance {-# OVERLAPS #-} FromJSON (ZohoResult () ()) where
--   parseJSON = zohoResultParser (const $ pure ()) (const $ pure ())

-- instance {-# OVERLAPS #-} (FromJSON details) => FromJSON (ZohoResult details ()) where
--   parseJSON = zohoResultParser parseJSON (const $ pure ())

-- instance (FromJSON details, FromJSON action) => FromJSON (ZohoResult details action) where
--   parseJSON = withObject "Expected object to parse into ZohoResult" $ \o -> do
--     zresCode <- o .: "code"
--     zres

-- zohoResultParser :: (FromJSON details)
--                  => (Aeson.Value -> Parser details)
--                  -> (Aeson.Value -> Parser upsert)
--                  -> Aeson.Value
--                  -> Parser (ZohoResult details upsert)
-- zohoResultParser dparser uparser v =
--   (flip (withObject "Expecting Object to parse into an UpserResult")) v $ \o -> do
--   zresCode <- (o .: "code") <|> (o .: "errorCode")
--   zresMessage <- o .: "message"
--   zresStatus <- (o .: "status") <|> (pure ZStatusError)
--   zresDetails <- dparser v
--   zresUpsertAction <- uparser v
--   pure ZohoResult{..}

$(deriveJSON (zohoPrefix pascalSnakeCase) ''VisitSummary)
$(deriveJSON (zohoPrefix pascalSnakeCase) ''ScoreSummary)
$(deriveJSON (zohoPrefix pascalSnakeCase) ''OnlyMetaData)
$(deriveJSON (zohoPrefix pascalSnakeCase) ''MetaData)
$(deriveJSON googleAdsJsonOptions ''GoogleAdsInfo)
$(deriveJSON (zohoPrefix Casing.snakeCase) ''Approval)
$(deriveJSON (zohoPrefix (('$':) . Casing.snakeCase)) ''SpecialFields)
-- $(deriveFromJSON (zohoPrefix Casing.snakeCase) ''ZohoResult)
$(makeLensesWith abbreviatedFields ''VisitSummary)
$(makeLensesWith abbreviatedFields ''ScoreSummary)
$(makeLensesWith abbreviatedFields ''MetaData)
$(makeLensesWith abbreviatedFields ''GoogleAdsInfo)
$(makeLensesWith abbreviatedFields ''OnlyMetaData)
$(makeLensesWith abbreviatedFields ''Approval)
$(makeLensesWith abbreviatedFields ''SpecialFields)
$(makeLensesWith abbreviatedFields ''ZohoResult)
