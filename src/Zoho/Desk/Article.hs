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
import Zoho.OAuth as ZO (prepareGet, applyOptionalQueryParam)
import Network.HTTP.Client as HC (Request)
import Zoho.Desk.Common as Common
import Zoho.ZohoM as ZM
import Data.String.Conv (toS)
import Control.Monad (join)
import Text.Read (readMaybe)
import Network.HTTP.Types as HT
import Zoho.Types

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
data ListOptions = ListOptions
  { optFrom :: !(Maybe Int)
  , optLimit :: !(Maybe Int)
  , optSortBy :: !(Maybe Text)  -- "createdTime", "modifiedTime", "likeCount", "viewCount", "feedbackCount", "dislikeCount", "positionArticle"
  , optPermission :: !(Maybe Text)  -- "ALL", "REGISTEREDUSERS", "AGENTS"
  , optAuthorId :: !(Maybe Text)
  , optCategoryId :: !(Maybe CategoryId)
  , optModifiedTimeRange :: !(Maybe Text)  -- ISO format: 'yyyy-MM-ddThh:mm:ss.SSSZ,yyyy-MM-ddThh:mm:ss.SSSZ'
  , optExpiryTimeRange :: !(Maybe Text)  -- ISO format
  , optStatus :: !(Maybe Text)  -- "Draft", "Published", "Review", "Expired", "Unpublished"
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyArticle :: Article
emptyArticle = emptyZohoStructure

emptyListOptions :: ListOptions
emptyListOptions = emptyZohoStructure

-- Generate lenses
$(makeLensesWith abbreviatedFields ''Article)
$(makeLensesWith abbreviatedFields ''ArticleUser)
$(makeLensesWith abbreviatedFields ''ArticleCategory)
$(makeLensesWith abbreviatedFields ''ArticleAttachment)
$(makeLensesWith abbreviatedFields ''ListOptions)

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


-- | Create request for listing articles
listArticlesRequest :: ListOptions -> OrgId -> Request
listArticlesRequest opts orgId =
  prepareGet (mkApiEndpoint "/articles") (buildArticleQuery opts) [orgIdHeader orgId]

-- | List all articles with optional filtering
listArticles :: HasZoho m => ListOptions -> OrgId -> m (Either Error [Article])
listArticles opts orgId = do
  x :: Either Error (ResponseWrapper "data" [Article]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listArticlesRequest opts orgId
  pure $ fmap unwrapResponse x

-- | Create request for getting a specific article by ID
getArticleRequest :: OrgId -> ArticleId -> Maybe Text -> Request
getArticleRequest orgId articleId maybeVersion =
  let endpoint = mkApiEndpoint $ "/articles/" <> toS articleId
      headers = [orgIdHeader orgId]
      queryParams = case maybeVersion of
        Nothing -> []
        Just version -> [("version", Just $ toS version)]
  in prepareGet endpoint queryParams headers

-- | Get a specific article by ID with optional version
getArticle :: HasZoho m => OrgId -> ArticleId -> Maybe Text -> m (Either Error Article)
getArticle orgId articleId maybeVersion =
  ZM.runRequestAndParseResponse $
  getArticleRequest orgId articleId maybeVersion

-- | Build query parameters for article listing (based on actual API spec)
buildArticleQuery :: ListOptions -> HT.Query
buildArticleQuery opts = 
  applyOptionalQueryParam "status" (optStatus opts) $
  applyOptionalQueryParam "categoryId" (optCategoryId opts) $
  applyOptionalQueryParam "sortBy" (optSortBy opts) $
  applyOptionalQueryParam "permission" (optPermission opts) $
  applyOptionalQueryParam "authorId" (optAuthorId opts) $
  applyOptionalQueryParam "modifiedTimeRange" (optModifiedTimeRange opts) $
  applyOptionalQueryParam "expiryTimeRange" (optExpiryTimeRange opts) $
  applyOptionalQueryParam "from" (show <$> optFrom opts) $
  applyOptionalQueryParam "limit" (show <$> optLimit opts) []