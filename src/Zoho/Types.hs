{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Zoho.Types where

import Data.Aeson
import GHC.TypeLits
import Data.Proxy
import Data.String.Conv
import Data.Aeson as Aeson
import Data.Text
import Data.Aeson.Casing as Casing
import Data.List as DL
import Data.Char as Char
import Network.HTTP.Client (HttpException)
import GHC.Generics
import Network.Wreq as W hiding (Proxy)
import Network.Wreq.Types as W (ResponseChecker)
import Network.HTTP.Client as HC (Request, Response, brConsume, HttpException(..), HttpExceptionContent(..))
import Debug.Trace
import Control.Lens ((^.))
import Control.Exception (throwIO, Exception)
import Data.Functor (void)
import Data.ByteString.Lazy as BSL
import Network.OAuth.OAuth2 (OAuth2Error)
import qualified Network.OAuth.OAuth2.TokenRequest as TokenRequest (Errors)

data ResponseWrapper (s :: Symbol) a = ResponseWrapper { unwrapResponse :: a } deriving (Eq, Show)

instance (FromJSON a, KnownSymbol s) => FromJSON (ResponseWrapper s a) where
  parseJSON = withObject "Expecting Object to parse into a ResponseWrapper"$ \o -> do
    r <- o .: (toS $ symbolVal (Proxy :: Proxy s))
    pure $ ResponseWrapper r

instance (ToJSON a, KnownSymbol s) => ToJSON (ResponseWrapper s a) where
  toJSON (ResponseWrapper v) =
    let k = symbolVal (Proxy :: Proxy s)
    in object [ (toS k) Aeson..= (toJSON v) ]

data PaginatedResponse (s :: Symbol) a = PaginatedResponse
  { pageData :: a
  , pageRecordsPerPage :: Int
  , pageTotalPages :: Int
  , pageCurrentPage :: Int
  , pageMoreRecords :: Bool
  } deriving (Eq, Show)


instance (FromJSON a, KnownSymbol s) => FromJSON (PaginatedResponse s a) where
  parseJSON = withObject "Expecting Object to parse into a PaginatedResponse" $ \o -> do
    pageData <- o .: (toS $ symbolVal (Proxy :: Proxy s))
    info_ <- o .: "info"
    pageRecordsPerPage <- info_ .: "per_page"
    pageTotalPages <- info_ .: "count"
    pageCurrentPage <- info_ .: "page"
    pageMoreRecords <- info_ .: "more_records"
    pure PaginatedResponse{..}

moduleJsonFieldNameMapping :: String -> String
moduleJsonFieldNameMapping s = case s of
  "EmailTemplateSupport" -> "emailTemplate_support"
  x -> snakeCase x

data Reference (s :: Symbol) = Reference
  { refId :: Text
  , refName :: Text
  } deriving (Eq, Show)

instance {-# OVERLAPS #-} (KnownSymbol s) => FromJSON (Maybe (Reference s)) where
  parseJSON v = case v of
    Aeson.Null -> pure Nothing
    Aeson.Object o -> if o == mempty
                      then pure Nothing
                      else fmap Just $ parseJSON v
    _ -> fmap Just $ parseJSON v

instance {-# OVERLAPS #-} (KnownSymbol s) => ToJSON (Maybe (Reference s)) where
  toJSON mRef = case mRef of
    Nothing -> object []
    Just ref -> toJSON ref

instance (KnownSymbol s) => FromJSON (Reference s) where
  parseJSON = withObject "Expecting Object to parse into a Reference" $ \o -> do
    refId <- o .: "id"
    refName <- o .: (toS $ symbolVal (Proxy :: Proxy s))
    pure Reference{..}


instance (KnownSymbol s) => ToJSON (Reference s) where
  toJSON Reference{..} = object
    [ "id" .= refId
    , (toS $ symbolVal (Proxy :: Proxy s)) .= refName
    ]

pascalSnakeCase :: String -> String
pascalSnakeCase s = go False s
  where
    go _ [] = []
    go isPrevLower (x:xs) = if Char.isLower x
                            then x:(go True xs)
                            else if isPrevLower
                                 then '_':x:(go False xs)
                                 else x:(go False xs)


class EmptyZohoStructure a where
  emptyZohoStructure :: a

  -- default emptyZohoStructure :: (Generic a, (GEmptyZohoStructure (Rep a))) => a
  -- emptyZohoStructure = GHC.Generics.to gEmptyZohoStructure

data Error = HTTPError !HttpException
           | ParseError !String !BSL.ByteString
           | TokenError (OAuth2Error TokenRequest.Errors)
           | OtherError !Aeson.Value
           deriving (Show)

instance Exception Error

data ZohoCode = ZCodeInvalidIToken
              | ZCodeSuccess
              | ZCodeOther !Text
              deriving (Eq, Show, Generic, Ord)

instance FromJSON ZohoCode where
  parseJSON = withText "Expecting text to parse into ZohoCode" $ \t -> pure $ case t of
    "INVALID_TOKEN" -> ZCodeInvalidIToken
    _ -> ZCodeOther t

data ZohoStatus = ZStatusError
                | ZStatusSuccess
                | ZStatusOther !Text
                deriving (Eq, Show, Generic, Ord)

instance FromJSON ZohoStatus where
  parseJSON = withText "Expecting text to parse into ZohoStatus" $ \t -> pure $ case t of
    "error" -> ZStatusError
    "success" -> ZStatusSuccess
    _ -> ZStatusOther t


data ZohoResult a = ZohoResult
  { zresCode :: !ZohoCode
  , zresDetails :: !a
  , zresMessage :: !Text
  , zresStatus :: !ZohoStatus
  } deriving (Eq, Show, Generic)

instance (FromJSON a) => FromJSON (ZohoResult a) where
  parseJSON = genericParseJSON (Casing.aesonPrefix snakeCase)

-- $(deriveJSON  ''ZohoErrro)


zohoResponseChecker :: W.ResponseChecker -- type ResponseChecker = Request -> Response BodyReader -> IO ()
zohoResponseChecker req res = do
  case res ^. W.responseStatus . W.statusCode of
    200 -> pure ()
    401 -> pure ()
    _ -> do
      bs <- HC.brConsume $ res ^. W.responseBody
      throwIO $ HttpExceptionRequest req $ StatusCodeException (void res) (mconcat bs)

data ZohoRetriableException = ZohoRetriableException deriving (Eq, Show)
instance Exception ZohoRetriableException
