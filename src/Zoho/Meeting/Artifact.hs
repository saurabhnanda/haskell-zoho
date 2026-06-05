-- | Downloading Zoho Meeting recording artifacts (video, and later transcript/summary).
--
-- The artifact URLs in a 'Zoho.Meeting.Recording.Recording' (@downloadUrl@,
-- @transcriptionDownloadUrl@, @summaryDownloadUrl@) are complete, pre-signed URLs --
-- typically on a NON-meeting host (e.g. @files-accl.zohopublic.com@). So we parse the
-- full URL directly rather than via @Zoho.Meeting.Common.mkApiEndpoint@, and let the
-- shared request runner attach the bearer token + handle the 401 refresh-retry.
module Zoho.Meeting.Artifact where

import Control.Monad.IO.Class (liftIO)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Network.HTTP.Client as HC
import Zoho.ZohoM as ZM (HasZoho, runRequestToFile)

-- | Authenticated download of a Zoho pre-signed media URL, streamed straight to @dest@
-- on disk -- the body is never buffered in memory (recordings run 50-200 MB+). This is
-- the only way to fetch a recording's media. Throws (via 'ZM.runRequestToFile') on a
-- non-2xx response; the file is written only on a 2xx.
downloadUrlToFile :: (HasZoho m) => Text -> FilePath -> m ()
downloadUrlToFile url dest = do
  req <- liftIO $ HC.parseRequest (toS url)
  ZM.runRequestToFile req dest
