module Zoho.Desk.Common where

import Data.Aeson as Aeson

data ErrorCode = ZInvalidToken
               | ZCodeOther !Text
                deriving (Eq, Sho, Ord)

instance FromJSON ErrorCode where
  parseJSON = withText "Expecting Text to parse into Zoho.Desk.Common.ErrorCode" $ \t -> pure $ case t of
    "INVALID_OAUTH" -> ZInvalidToken
    _ -> ZCodeOther t

data ZohoResult = ZohoResult
  { resErrorCode :: !ErrorCode
  , resMessage :: !(Maybe Text)
  } deriving (Eq, Show)

instance FromJSON ZohoResult where
  parseJSON = withObject "Expecting Object to parse into Zoho.Desk.Common.ZohoResult" $ \o -> do
    resErrorCode <- o .: "errorCode"
    resMessage <- o .: "message"
    pure ZohoResult{..}
