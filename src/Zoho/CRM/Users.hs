{-# LANGUAGE DeriveAnyClass #-}
module Zoho.CRM.Users where

import Zoho.OAuth as ZO
import Network.OAuth.OAuth2 as O
-- import Network.Wreq as W hiding (Proxy(..))
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
-- import Network.Wreq as W
import Data.Aeson as Aeson
import Data.String.Conv
import Network.HTTP.Client (Manager)
import URI.ByteString as U
import Zoho.Types
import qualified Data.Text as T
import Data.Text (Text)
import Control.Lens
import Zoho.CRM.Common
import Data.Time
import Data.Maybe (listToMaybe)
import Zoho.ZohoM as ZM
import Network.HTTP.Client as HC
import Network.HTTP.Types as HT
import Data.Aeson.TH
import Data.Aeson.Casing as Casing
import qualified Data.List.NonEmpty as NE
import GHC.Base (sconcat)
import Data.Text.Conversions (ToText(..))
import GHC.Generics

type UserId = Text

apiEndpoint :: BS.ByteString -> URI
apiEndpoint path_  = ZO.mkApiEndpoint $ "/crm/v2/users" <> path_

-- data User = User
--   { userId :: _
--   , 
    
--   }

data ListUserType = AllUsers
                  | ActiveUsers
                  | DeactiveUsers
                  | ConfirmedUsers
                  | NotConfirmedUsers
                  | DeletedUsers
                  | ActiveConfirmedUsers
                  | AdminUsers
                  | ActiveConfirmedAdmins
                  | CurrentUser
                  deriving (Eq, Show, Ord, Enum)

list :: (FromJSON a, HasZoho m)
     => ListUserType
     -> m (Either Error (PaginatedResponse "users" [a]))
list utype =
  ZM.runRequestAndParseOptionalResponse emptyPaginatedResponse Prelude.id $
  listRequest utype


listRequest :: ListUserType
            -> Request
listRequest utype =
  ZO.prepareGet (apiEndpoint "") qparams []
  where
    qparams = [("type", Just $ toS $ show utype)]

