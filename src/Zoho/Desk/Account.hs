{-# LANGUAGE DeriveAnyClass #-}
module Zoho.Desk.Account where

import Control.Lens
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.Aeson.Casing as Casing
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Zoho.Desk.Utils (accountJsonOptions)
import Zoho.Types (EmptyZohoStructure(..))

data Account cf = Account
  { accId :: !(Maybe Text)
  , accAccountName :: !(Maybe Text)
  , accEmail :: !(Maybe Text)
  , accPhone :: !(Maybe Text)
  , accWebsite :: !(Maybe Text)
  , accFax :: !(Maybe Text)
  , accStreet :: !(Maybe Text)
  , accCity :: !(Maybe Text)
  , accState :: !(Maybe Text)
  , accCountry :: !(Maybe Text)
  , accIndustry :: !(Maybe Text)
  , accCode :: !(Maybe Text)
  , accDescription :: !(Maybe Text)
  , accAnnualRevenue :: !(Maybe Text) -- Strangely the UI validates this to be a number, but the JSON sends it as a string.
  , accCreatedTime :: !(Maybe UTCTime)
  , accModifiedTime :: !(Maybe UTCTime)
  , accZohoCRMAccount :: Maybe ()
  , accWebUrl :: !(Maybe Text)
  , accAssociatedSLAIds :: Maybe ()
  , accFollowing :: !(Maybe Bool)
  , accDeleted :: !(Maybe Bool)
  , accTrashed :: !(Maybe Bool)
  , accCustomFields :: !(Maybe cf)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyAccount :: Account cf
emptyAccount = emptyZohoStructure

$(deriveJSON accountJsonOptions ''Account)
