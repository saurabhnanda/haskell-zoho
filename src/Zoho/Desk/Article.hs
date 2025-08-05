{-# LANGUAGE DeriveAnyClass #-}
module Zoho.Desk.Article
  ( module Zoho.Desk.Article
  , module Common
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Zoho.Types
import GHC.Generics
import qualified Data.Aeson.Casing as Casing
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Control.Lens (makeLensesWith, abbreviatedFields)
import Zoho.OAuth as ZO
import Network.HTTP.Client as HC (Request)
import Zoho.Desk.Common as Common
import Zoho.ZohoM as ZM
import Data.String.Conv (toS)
import Control.Monad (join)
import Text.Read (readMaybe)
import qualified Data.Vector as V
import Network.HTTP.Types as HT

type ArticleId = Text
type CategoryId = Text
type AuthorId = Text
type DepartmentId = Text
type TranslationId = Text

-- | User object (used for author, owner, etc.)
data ArticleUser = ArticleUser
  { auPhotoURL :: !(Maybe Text)
  , auName :: !(Maybe Text)
  , auId :: !(Maybe Text)
  , auStatus :: !(Maybe Text)
  , auZuid :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

-- | Category object
data ArticleCategory = ArticleCategory
  { acName :: !(Maybe Text)
  , acId :: !(Maybe CategoryId)
  , acLocale :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

-- | Article attachment object
data ArticleAttachment = ArticleAttachment
  { aaResourceId :: !(Maybe Text)
  , aaExtension :: !(Maybe Text)
  , aaSize :: !(Maybe Text)
  , aaName :: !(Maybe Text)
  , aaDownloadUrl :: !(Maybe Text)
  , aaViewUrl :: !(Maybe Text)
  , aaCreatedTime :: !(Maybe UTCTime)
  } deriving (Eq, Show, Generic)

-- | Represents a knowledge base article
data Article = Article
  { articleId :: !(Maybe ArticleId)
  , articleTitle :: !(Maybe Text)
  , articleAnswer :: !(Maybe Text)  -- Full HTML content
  , articleSummary :: !(Maybe Text)
  , articlePermalink :: !(Maybe Text)
  , articleStatus :: !(Maybe Text)  -- "Published", "Draft", etc.
  , articleCategoryId :: !(Maybe CategoryId)
  , articleCategory :: !(Maybe ArticleCategory)
  , articleViewCount :: !(Maybe Text)  -- API returns as string
  , articleLikeCount :: !(Maybe Text)  -- API returns as string
  , articleDislikeCount :: !(Maybe Text)  -- API returns as string
  , articlePermission :: !(Maybe Text)  -- "AGENTS", "ALL", "REGISTEREDUSERS"
  , articleCreatedTime :: !(Maybe UTCTime)
  , articleModifiedTime :: !(Maybe UTCTime)
  , articleAuthor :: !(Maybe ArticleUser)
  , articleAuthorId :: !(Maybe AuthorId)
  , articleOwner :: !(Maybe ArticleUser)
  , articleOwnerId :: !(Maybe AuthorId)
  , articleCreatedBy :: !(Maybe ArticleUser)
  , articleModifiedBy :: !(Maybe ArticleUser)
  , articleTags :: !(Maybe [Text])
  , articleWebUrl :: !(Maybe Text)
  , articlePortalUrl :: !(Maybe Text)
  , articleDepartmentId :: !(Maybe DepartmentId)
  , articleLocale :: !(Maybe Text)
  , articleUsageCount :: !(Maybe Text)
  , articleCommentCount :: !(Maybe Text)
  , articleAttachmentCount :: !(Maybe Text)
  , articleFeedbackCount :: !(Maybe Text)
  , articleTemplateUsageCount :: !(Maybe Text)
  , articlePosition :: !(Maybe Text)
  , articleLatestVersion :: !(Maybe Text)
  , articleLatestPublishedVersion :: !(Maybe Text)
  , articleCurrentVersion :: !(Maybe Text)
  , articleIsTemplate :: !(Maybe Bool)
  , articleIsTrashed :: !(Maybe Bool)
  , articleTranslationState :: !(Maybe Text)
  , articleTranslationId :: !(Maybe TranslationId)
  , articleAttachments :: !(Maybe [ArticleAttachment])
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

-- | Options for listing articles (based on actual API spec)
data ListArticlesOptions = ListArticlesOptions
  { laoFrom :: !(Maybe Int)
  , laoLimit :: !(Maybe Int)
  , laoSortBy :: !(Maybe Text)  -- "createdTime", "modifiedTime", "likeCount", "viewCount", "feedbackCount", "dislikeCount", "positionArticle"
  , laoPermission :: !(Maybe Text)  -- "ALL", "REGISTEREDUSERS", "AGENTS"
  , laoAuthorId :: !(Maybe Text)
  , laoCategoryId :: !(Maybe CategoryId)
  , laoModifiedTimeRange :: !(Maybe Text)  -- ISO format: 'yyyy-MM-ddThh:mm:ss.SSSZ,yyyy-MM-ddThh:mm:ss.SSSZ'
  , laoExpiryTimeRange :: !(Maybe Text)  -- ISO format
  , laoStatus :: !(Maybe Text)  -- "Draft", "Published", "Review", "Expired", "Unpublished"
  } deriving (Eq, Show, Generic)

emptyArticle :: Article
emptyArticle = emptyZohoStructure

emptyListArticlesOptions :: ListArticlesOptions
emptyListArticlesOptions = ListArticlesOptions Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- Generate lenses
$(makeLensesWith abbreviatedFields ''Article)
$(makeLensesWith abbreviatedFields ''ArticleUser)
$(makeLensesWith abbreviatedFields ''ArticleCategory)
$(makeLensesWith abbreviatedFields ''ArticleAttachment)
$(makeLensesWith abbreviatedFields ''ListArticlesOptions)

-- JSON options for articles
articleJsonOptions :: Aeson.Options
articleJsonOptions = zohoPrefix Casing.camelCase

articleUserJsonOptions :: Aeson.Options
articleUserJsonOptions = zohoPrefix Casing.camelCase

articleCategoryJsonOptions :: Aeson.Options
articleCategoryJsonOptions = zohoPrefix Casing.camelCase

articleAttachmentJsonOptions :: Aeson.Options
articleAttachmentJsonOptions = zohoPrefix Casing.camelCase

-- JSON instances
instance FromJSON Article where
  parseJSON = genericParseJSON articleJsonOptions

instance ToJSON Article where
  toJSON = genericToJSON articleJsonOptions

instance FromJSON ArticleUser where
  parseJSON = genericParseJSON articleUserJsonOptions

instance ToJSON ArticleUser where
  toJSON = genericToJSON articleUserJsonOptions

instance FromJSON ArticleCategory where
  parseJSON = genericParseJSON articleCategoryJsonOptions

instance ToJSON ArticleCategory where
  toJSON = genericToJSON articleCategoryJsonOptions

instance FromJSON ArticleAttachment where
  parseJSON = genericParseJSON articleAttachmentJsonOptions

instance ToJSON ArticleAttachment where
  toJSON = genericToJSON articleAttachmentJsonOptions

-- Lens class instances for common search parameters
instance HasFrom ListArticlesOptions (Maybe Int) where from = laoFrom
instance HasLimit ListArticlesOptions (Maybe Int) where limit = laoLimit
instance HasId ListArticlesOptions (Maybe Text) where id = const (const emptyListArticlesOptions)
instance HasAll ListArticlesOptions (Maybe Bool) where all = const (const emptyListArticlesOptions)
instance HasCreatedTimeRange ListArticlesOptions (Maybe (UTCTime, UTCTime)) where createdTimeRange = const (const emptyListArticlesOptions)
instance HasModifiedTimeRange ListArticlesOptions (Maybe (UTCTime, UTCTime)) where modifiedTimeRange = const (const emptyListArticlesOptions)

-- | List all articles with optional filtering
listArticles :: ZohoM m => OrgId -> ListArticlesOptions -> m (SearchResults Article)
listArticles orgId opts = do
  let endpoint = mkApiEndpoint "/articles"
      headers = [orgIdHeader orgId]
      queryParams = buildArticleQuery opts
  
  ZM.get endpoint headers queryParams

-- | Get a specific article by ID with optional version
getArticle :: ZohoM m => OrgId -> ArticleId -> Maybe Text -> m Article
getArticle orgId articleId maybeVersion = do
  let endpoint = mkApiEndpoint $ "/articles/" <> toS articleId
      headers = [orgIdHeader orgId]
      queryParams = case maybeVersion of
        Nothing -> []
        Just version -> [("version", Just $ toS version)]
  
  ZM.get endpoint headers queryParams

-- | Build query parameters for article listing (based on actual API spec)
buildArticleQuery :: ListArticlesOptions -> HT.Query
buildArticleQuery opts = 
  applyOptionalQueryParam "status" (laoStatus opts) $
  applyOptionalQueryParam "categoryId" (laoCategoryId opts) $
  applyOptionalQueryParam "sortBy" (laoSortBy opts) $
  applyOptionalQueryParam "permission" (laoPermission opts) $
  applyOptionalQueryParam "authorId" (laoAuthorId opts) $
  applyOptionalQueryParam "modifiedTimeRange" (laoModifiedTimeRange opts) $
  applyOptionalQueryParam "expiryTimeRange" (laoExpiryTimeRange opts) $
  applyOptionalQueryParam "from" (show <$> laoFrom opts) $
  applyOptionalQueryParam "limit" (show <$> laoLimit opts) []

-- | Helper to apply optional query parameters
applyOptionalQueryParam :: ToS a => HT.HeaderName -> Maybe a -> HT.Query -> HT.Query
applyOptionalQueryParam _ Nothing query = query
applyOptionalQueryParam key (Just value) query = (key, Just $ toS value) : query