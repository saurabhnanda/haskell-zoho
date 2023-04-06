module Zoho.Books.Common where

import qualified Data.ByteString as BS
import Zoho.Types
import Data.String.Conv
import qualified Zoho.OAuth as ZO
import URI.ByteString as U

mkApiEndpoint :: BS.ByteString -> URI
mkApiEndpoint p = ZO.mkEndpoint (Host "www.zohoapis.com") ("/books/v3" <> p)

orgIdParam :: OrgId -> [(BS.ByteString, Maybe BS.ByteString)]
orgIdParam (OrgId oid) = [("organization_id", Just $ toS oid)]
