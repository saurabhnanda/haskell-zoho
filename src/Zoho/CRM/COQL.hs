module Zoho.CRM.COQL where

import qualified Zoho.OAuth as ZO
import Zoho.ZohoM as ZM
import Zoho.Types
import Zoho.CRM.Common
import Data.Text (Text)
import Data.Aeson as Aeson
import Zoho.Types (PaginatedResponse)

coql :: (HasZoho m, FromJSON a)
     => Text
     -> m (Either Error (PaginatedResponse "data" [a]))
coql qry =
  ZM.runRequestAndParseOptionalResponse emptyPaginatedResponse Prelude.id $
  ZO.prepareJSONPost (ZO.mkApiEndpoint "/crm/v2/coql") [] [] pload
  where
    pload = Aeson.object [ "select_query" Aeson..= qry ]
