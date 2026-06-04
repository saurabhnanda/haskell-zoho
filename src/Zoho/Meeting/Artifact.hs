{-# LANGUAGE OverloadedStrings #-}

-- | Downloading Zoho Meeting recording artifacts (video, and later transcript/summary).
--
-- The artifact URLs in a 'Zoho.Meeting.Recording.Recording' (@downloadUrl@,
-- @transcriptionDownloadUrl@, @summaryDownloadUrl@) are complete, pre-signed URLs --
-- typically on a NON-meeting host (e.g. @files-accl.zohopublic.com@). So we parse the
-- full URL directly rather than via @Zoho.Meeting.Common.mkApiEndpoint@, and let the
-- shared 'ZM.runRequest' attach the bearer token + handle the 401 refresh-retry.
module Zoho.Meeting.Artifact where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BSL
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Network.HTTP.Client as HC
import Zoho.ZohoM as ZM (HasZoho, runRequest)

-- | Authenticated download of a Zoho pre-signed media URL, returning the raw bytes.
-- Throws (via 'runRequest') on a non-2xx response. The body is a lazy 'BSL.ByteString',
-- so the caller can stream it straight to disk.
downloadUrl :: (HasZoho m) => Text -> m BSL.ByteString
downloadUrl url = do
  req <- liftIO $ HC.parseRequest (toS url)
  HC.responseBody <$> ZM.runRequest req
