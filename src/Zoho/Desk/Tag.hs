{-# LANGUAGE DeriveAnyClass #-}
module Zoho.Desk.Tag
  ( module Zoho.Desk.Tag
  , module Common
  )
where

import Control.Lens (makeLensesWith, abbreviatedFields)
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Zoho.Types (EmptyZohoStructure(..), Error, zohoPrefix)
import Zoho.OAuth as ZO hiding (mkApiEndpoint)
import Zoho.Desk.Common as Common
import Network.HTTP.Client as HC (Request)
import Zoho.ZohoM as ZM
import Prelude
import Data.String.Conv (toS)

-- | Tag type classification
data TagType = ManualTag | AutoTag | TagTypeOther !Text
  deriving (Eq, Show)

instance FromJSON TagType where
  parseJSON = withText "Expecting Text to parse into TagType" $ \t ->
    pure $ case T.toUpper t of
      "MANUAL" -> ManualTag
      "AUTO" -> AutoTag
      x -> TagTypeOther x

instance ToJSON TagType where
  toJSON ManualTag = toJSON ("MANUAL" :: Text)
  toJSON AutoTag = toJSON ("AUTO" :: Text)
  toJSON (TagTypeOther t) = toJSON t

-- | Tag data type based on Zoho Desk API response
data Tag = Tag
  { tagId :: !(Maybe TagId)
  , tagName :: !(Maybe Text)
  , tagTagType :: !(Maybe TagType)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyTag :: Tag
emptyTag = emptyZohoStructure

$(makeLensesWith abbreviatedFields ''Tag)

instance FromJSON Tag where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

instance ToJSON Tag where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

-- | Build request for listing tags in a specific ticket
listRequest :: OrgId -> TicketId -> Request
listRequest orgId ticketId =
  ZO.prepareGet (Common.mkApiEndpoint ("/tickets/" <> toS ticketId <> "/tags")) [] [Common.orgIdHeader orgId]

-- | List all tags for a specific ticket
-- GET /api/v1/tickets/{ticket_id}/tags
list :: (HasZoho m) => OrgId -> TicketId -> m (Either Error [Tag])
list orgId ticketId = do
  x :: Either Error (ResponseWrapper "tags" [Tag]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listRequest orgId ticketId
  pure $ fmap unwrapResponse x


-- * Associate tags with ticket

-- | Request body for associateTag
newtype AssociateTagRequest = AssociateTagRequest
  { atrTags :: [Text]
  } deriving (Eq, Show, Generic)

instance ToJSON AssociateTagRequest where
  toJSON = genericToJSON (Casing.aesonPrefix Casing.camelCase)

-- | Build request for associating tags with a ticket
associateRequest :: OrgId -> TicketId -> [Text] -> Request
associateRequest orgId ticketId tagNames =
  let requestBody = AssociateTagRequest tagNames
  in ZO.prepareJSONPost (Common.mkApiEndpoint $ "/tickets/" <> toS ticketId <> "/associateTag") [] [Common.orgIdHeader orgId] requestBody

-- | Associate tags with a ticket
-- POST /api/v1/tickets/{ticket_id}/associateTag
-- OAuth Scope: Desk.tickets.CREATE
--
-- Note: The response contains the full tag objects including IDs and types.
-- The API will create new tags if they don't exist.
associate :: (HasZoho m)
          => OrgId
          -> TicketId
          -> [Text]  -- ^ List of tag names to associate
          -> m (Either Error [Tag])
associate orgId ticketId tagNames = do
  result :: Either Error (ResponseWrapper "data" [Tag]) <-
    ZM.runRequestAndParseResponse $
    associateRequest orgId ticketId tagNames
  pure $ fmap unwrapResponse result


-- * Dissociate tags from ticket

-- | Build request for dissociating tags from a ticket. Reuses 'AssociateTagRequest'
-- -- the dissociate endpoint takes the same @{"tags":[...]}@ body.
dissociateRequest :: OrgId -> TicketId -> [Text] -> Request
dissociateRequest orgId ticketId tagNames =
  let requestBody = AssociateTagRequest tagNames
  in ZO.prepareJSONPost (Common.mkApiEndpoint $ "/tickets/" <> toS ticketId <> "/dissociateTag") [] [Common.orgIdHeader orgId] requestBody

-- | Dissociate tags from a ticket
-- POST /api/v1/tickets/{ticket_id}/dissociateTag
-- OAuth Scope: Desk.tickets.CREATE
--
-- Removes one or more tags (by name) from the ticket. Returns @200@; the body may
-- be empty, so we parse optionally and default to an empty tag list.
dissociate :: (HasZoho m)
           => OrgId
           -> TicketId
           -> [Text]  -- ^ List of tag names to dissociate
           -> m (Either Error [Tag])
dissociate orgId ticketId tagNames = do
  result :: Either Error (ResponseWrapper "data" [Tag]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    dissociateRequest orgId ticketId tagNames
  pure $ fmap unwrapResponse result
