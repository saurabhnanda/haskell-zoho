{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Zoho Projects v3 task comments: add a comment to a task.
--
-- Two shapes, kept separate (the create request is NOT the read entity), mirroring
-- 'Zoho.Desk.Conversation' (@CreateCommentRequest@ + @CreatedEntry@): the input is
-- 'AddTaskCommentRequest' (just the comment text), the output is decoded leniently as a
-- minimal 'CreatedComment' (just the new comment id -- all we need to confirm the write).
-- The create endpoint echoes the comment inside a JSON ARRAY, so 'addTaskComment' returns
-- @[CreatedComment]@.
--
-- User @mentions ride INSIDE the comment text using the token @zp[@zpuser#<zuid>]zp@
-- (the zuid-only form -- Zoho's recommended, most reliable one; @<zuid>@ is the global
-- 'Zoho.Types.Zuid', the same id used for owner assignment).
module Zoho.Projects.Comment
  ( AddTaskCommentRequest(..)
  , CreatedComment(..)
  , addTaskCommentRequest
  , addTaskComment
  ) where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics
import Zoho.Projects.Common (PortalId, ProjectId (..), TaskId (..), CommentId, mkApiEndpoint)
import qualified Zoho.OAuth as ZO
import Zoho.Types (Error)
import Zoho.ZohoM as ZM (HasZoho, runRequestAndParseResponse)

-- | Request body for adding a task comment. Only the comment text is modelled; Zoho's
-- optional @attachments@ / @thirdparty_id@ inputs are omitted (add when needed).
newtype AddTaskCommentRequest = AddTaskCommentRequest
  { atcrComment :: Text  -- ^ comment -- plain text or HTML (<= 64K); embed mentions as @zp[@zpuser#<zuid>]zp@
  } deriving (Eq, Show, Generic)

instance ToJSON AddTaskCommentRequest where
  toJSON = genericToJSON (aesonPrefix snakeCase)

-- | Convenience constructor for the common case (a comment with no attachments).
addTaskCommentRequest :: Text -> AddTaskCommentRequest
addTaskCommentRequest = AddTaskCommentRequest

-- | Minimal decode of the create-comment response: just the new comment's id. Mirrors
-- 'Zoho.Desk.Conversation.CreatedEntry' -- we only need to confirm the write succeeded
-- (the full comment object carries many read-only fields we do not use).
newtype CreatedComment = CreatedComment { ccId :: Maybe CommentId }
  deriving (Eq, Show, Generic)

instance FromJSON CreatedComment where
  parseJSON = withObject "CreatedComment" $ \o -> CreatedComment <$> o .:? "id"

-- | Add a comment to a task. The v3 API echoes the created comment inside a JSON ARRAY
-- (normally a single element), so this returns @[CreatedComment]@.
addTaskComment :: (HasZoho m) => PortalId -> ProjectId -> TaskId -> AddTaskCommentRequest -> m (Either Error [CreatedComment])
addTaskComment portalId (ProjectId pid) (TaskId tid) body =
  runRequestAndParseResponse $
    ZO.prepareJSONPost (mkApiEndpoint portalId ("/projects/" <> toS pid <> "/tasks/" <> toS tid <> "/comments")) [] [] body
