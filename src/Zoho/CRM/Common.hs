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

data ScoreSummary = ScoreSummary
  { ssScore :: Maybe Int
  , ssPositiveScore :: Maybe Int
  , ssNegativeScore :: Maybe Int
  , ssTouchPointScore :: Maybe Int
  , ssPositiveTouchPointScore :: Maybe Int
  , ssNegativeTouchPointScore :: Maybe Int
  } deriving (Eq, Show)

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
  } deriving (Show)

$(deriveJSON (Casing.aesonPrefix pascalSnakeCase) ''VisitSummary)
$(deriveJSON (Casing.aesonPrefix pascalSnakeCase) ''ScoreSummary)
$(deriveJSON googleAdsJsonOptions ''GoogleAdsInfo)
