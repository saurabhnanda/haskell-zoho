{-# LANGUAGE DeriveAnyClass #-}
module Zoho.Desk.KbCategory
  ( module Zoho.Desk.KbCategory
  , module Common
  )
where

import Data.Text (Text)
import Data.Time
import Zoho.Types
import GHC.Generics
import qualified Data.Aeson.Casing as Casing
import Data.Aeson as Aeson
import Control.Lens (makeLensesWith, abbreviatedFields)
import Zoho.OAuth as ZO (prepareGet, applyOptionalQueryParam)
import Network.HTTP.Client as HC (Request)
import Zoho.Desk.Common as Common (mkApiEndpoint, orgIdHeader)
import Zoho.ZohoM as ZM
import Data.String.Conv (toS)
import Network.HTTP.Types as HT

-- | Translation object (shared by root categories and tree nodes)
data KbTranslation = KbTranslation
  { kbtId :: !(Maybe Text)
  , kbtName :: !(Maybe Text)
  , kbtDescription :: !(Maybe Text)
  , kbtPermalink :: !(Maybe Text)
  , kbtLocale :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

-- | Root KB category (top-level grouping)
data KbRootCategory = KbRootCategory
  { kbrcId :: !(Maybe Text)
  , kbrcName :: !(Maybe Text)
  , kbrcOrder :: !(Maybe Text)
  , kbrcStatus :: !(Maybe Text)
  , kbrcDescription :: !(Maybe Text)
  , kbrcSectionsCount :: !(Maybe Text)
  , kbrcPublicArticlesCount :: !(Maybe Text)
  , kbrcAllArticlesCount :: !(Maybe Text)
  , kbrcLogoUrl :: !(Maybe Text)
  , kbrcTranslations :: !(Maybe [KbTranslation])
  , kbrcCreatedTime :: !(Maybe UTCTime)
  , kbrcModifiedTime :: !(Maybe UTCTime)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

-- | Node in a category tree (sections and sub-sections)
data KbCategoryTreeNode = KbCategoryTreeNode
  { kbctnId :: !(Maybe Text)
  , kbctnName :: !(Maybe Text)
  , kbctnOrder :: !(Maybe Text)
  , kbctnStatus :: !(Maybe Text)
  , kbctnDescription :: !(Maybe Text)
  , kbctnParentCategoryId :: !(Maybe Text)
  , kbctnRootCategoryId :: !(Maybe Text)
  , kbctnPublicArticlesCount :: !(Maybe Text)
  , kbctnAllArticlesCount :: !(Maybe Text)
  , kbctnLogoUrl :: !(Maybe Text)
  , kbctnTranslations :: !(Maybe [KbTranslation])
  , kbctnChildren :: !(Maybe [KbCategoryTreeNode])
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

-- Generate lenses
$(makeLensesWith abbreviatedFields ''KbTranslation)
$(makeLensesWith abbreviatedFields ''KbRootCategory)
$(makeLensesWith abbreviatedFields ''KbCategoryTreeNode)

-- JSON options
kbTranslationJsonOptions :: Aeson.Options
kbTranslationJsonOptions = zohoPrefix Casing.camelCase

kbRootCategoryJsonOptions :: Aeson.Options
kbRootCategoryJsonOptions = zohoPrefix Casing.camelCase

kbCategoryTreeNodeJsonOptions :: Aeson.Options
kbCategoryTreeNodeJsonOptions = zohoPrefix Casing.camelCase

-- JSON instances
instance FromJSON KbTranslation where
  parseJSON = genericParseJSON kbTranslationJsonOptions

instance ToJSON KbTranslation where
  toJSON = genericToJSON kbTranslationJsonOptions

instance FromJSON KbRootCategory where
  parseJSON = genericParseJSON kbRootCategoryJsonOptions

instance ToJSON KbRootCategory where
  toJSON = genericToJSON kbRootCategoryJsonOptions

instance FromJSON KbCategoryTreeNode where
  parseJSON = genericParseJSON kbCategoryTreeNodeJsonOptions

instance ToJSON KbCategoryTreeNode where
  toJSON = genericToJSON kbCategoryTreeNodeJsonOptions

-- | Create request for listing root KB categories
listRootCategoriesRequest :: OrgId -> Request
listRootCategoriesRequest orgId =
  prepareGet
    (mkApiEndpoint "/kbRootCategories")
    [ ("sortBy", Just "order")
    , ("include", Just "sectionsCount,publicArticlesCount,allArticlesCount")
    , ("limit", Just "700")
    ]
    [orgIdHeader orgId]

-- | List all root KB categories (sorted by order)
listRootCategories :: HasZoho m => OrgId -> m (Either Error [KbRootCategory])
listRootCategories orgId = do
  x :: Either Error (ResponseWrapper "data" [KbRootCategory]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listRootCategoriesRequest orgId
  pure $ fmap unwrapResponse x

-- | Create request for getting a category tree
getCategoryTreeRequest :: OrgId -> Text -> Request
getCategoryTreeRequest orgId rootCategoryId =
  prepareGet
    (mkApiEndpoint $ "/kbRootCategories/" <> toS rootCategoryId <> "/categoryTree")
    [ ("sortBy", Just "order")
    , ("include", Just "publicArticlesCount,allArticlesCount")
    ]
    [orgIdHeader orgId]

-- | Get the full category tree for a root category
getCategoryTree :: HasZoho m => OrgId -> Text -> m (Either Error KbCategoryTreeNode)
getCategoryTree orgId rootCategoryId =
  ZM.runRequestAndParseResponse $
  getCategoryTreeRequest orgId rootCategoryId
