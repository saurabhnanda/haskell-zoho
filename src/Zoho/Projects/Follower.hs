{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Zoho Projects v3 task followers: add one or more followers to a task.
--
-- Followers are keyed by 'Zpuid' (portal-specific), NOT the global 'Zoho.Types.Zuid' that
-- owner-assignment uses -- see 'Zoho.Projects.Common.Zpuid'.
--
-- The response is PARTIAL-SUCCESS: it carries both a @followers@ array (those added) and
-- an @errors@ array (per-zpuid failures, e.g. a permission error). Both are decoded --
-- 'runRequestAndParseResponse' does not treat an @errors@ key as a transport error, so a
-- caller must inspect 'afresErrors' to detect a rejected follower.
module Zoho.Projects.Follower
  ( FollowerRef(..)
  , AddFollowersRequest(..)
  , TaskFollower(..)
  , FollowerError(..)
  , AddFollowersResponse(..)
  , addFollowersRequest
  , addFollowers
  ) where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics
import Zoho.Projects.Common (PortalId, ProjectId (..), TaskId (..), Zpuid, mkApiEndpoint)
import qualified Zoho.OAuth as ZO
import Zoho.Types (Error)
import Zoho.ZohoM as ZM (HasZoho, runRequestAndParseResponse)

-- | One follower entry in the request array. Serializes to @{"zpuid": "..."}@.
newtype FollowerRef = FollowerRef { frZpuid :: Zpuid }
  deriving (Eq, Show, Generic)

instance ToJSON FollowerRef where
  toJSON = genericToJSON (aesonPrefix snakeCase)

-- | Request body: @{"followers": [{"zpuid": "..."}]}@.
newtype AddFollowersRequest = AddFollowersRequest { afreqFollowers :: [FollowerRef] }
  deriving (Eq, Show, Generic)

instance ToJSON AddFollowersRequest where
  toJSON = genericToJSON (aesonPrefix snakeCase)

-- | Convenience constructor from a list of zpuids.
addFollowersRequest :: [Zpuid] -> AddFollowersRequest
addFollowersRequest = AddFollowersRequest . map FollowerRef

-- | One successfully-added follower, echoed back. Zoho also returns @zuid@ / @first_name@
-- / @last_name@ (ignored here).
data TaskFollower = TaskFollower
  { tfZpuid :: !(Maybe Zpuid)
  , tfName  :: !(Maybe Text)
  , tfEmail :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON TaskFollower where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)

-- | One per-follower failure from the response @errors@ array (e.g. @message_key@
-- @zp.followers.taskperm@ when the caller lacks permission to add followers).
data FollowerError = FollowerError
  { feFieldValue :: !(Maybe Text)
  , feMessage    :: !(Maybe Text)
  , feMessageKey :: !(Maybe Text)
  , feFieldName  :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON FollowerError where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)

-- | Partial-success response: @followers@ = added, @errors@ = rejected. Either array may
-- be absent (defaulted to empty).
data AddFollowersResponse = AddFollowersResponse
  { afresFollowers :: ![TaskFollower]
  , afresErrors    :: ![FollowerError]
  } deriving (Eq, Show, Generic)

instance FromJSON AddFollowersResponse where
  parseJSON = withObject "AddFollowersResponse" $ \o ->
    AddFollowersResponse <$> (o .:? "followers" .!= []) <*> (o .:? "errors" .!= [])

-- | Add followers to a task. Inspect 'afresErrors' in the result to detect a rejected
-- follower (a non-empty @errors@ array is NOT surfaced as a 'Left').
addFollowers :: (HasZoho m) => PortalId -> ProjectId -> TaskId -> AddFollowersRequest -> m (Either Error AddFollowersResponse)
addFollowers portalId (ProjectId pid) (TaskId tid) body =
  runRequestAndParseResponse $
    ZO.prepareJSONPost (mkApiEndpoint portalId ("/projects/" <> toS pid <> "/tasks/" <> toS tid <> "/followers")) [] [] body
