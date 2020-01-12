{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
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
import Control.Lens hiding ((.=), to)

type ApiName = Text

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
  { pageActualData :: a
  , pageRecordsPerPage :: Int
  , pageTotalPages :: Int
  , pageCurrentPage :: Int
  , pageMoreRecords :: Bool
  } deriving (Eq, Show)
$(makeLensesWith abbreviatedFields ''PaginatedResponse)

instance (FromJSON a, KnownSymbol s) => FromJSON (PaginatedResponse s a) where
  parseJSON = withObject "Expecting Object to parse into a PaginatedResponse" $ \o -> do
    pageActualData <- o .: (toS $ symbolVal (Proxy :: Proxy s))
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


  -- default emptyZohoStructure :: (Generic a, (GEmptyZohoStructure (Rep a))) => a
  -- emptyZohoStructure = GHC.Generics.to gEmptyZohoStructure

data Error = HTTPError !HttpException
           | ParseError !String !BSL.ByteString
           | TokenError (OAuth2Error TokenRequest.Errors)
           | OtherError !Aeson.Value
           deriving (Show)

instance Exception Error


-- $(deriveJSON  ''ZohoErrro)


-- zohoResponseChecker :: W.ResponseChecker -- type ResponseChecker = Request -> Response BodyReader -> IO ()
-- zohoResponseChecker req res = do
--   case res ^. W.responseStatus . W.statusCode of
--     200 -> pure ()
--     401 -> pure ()
--     _ -> do
--       bs <- HC.brConsume $ res ^. W.responseBody
--       throwIO $ HttpExceptionRequest req $ StatusCodeException (void res) (mconcat bs)

data ZohoRetriableException = ZohoRetriableException deriving (Eq, Show)
instance Exception ZohoRetriableException


unsafeMergeObjects :: Aeson.Value -> Aeson.Value -> Aeson.Value
unsafeMergeObjects (Aeson.Object x) (Aeson.Object y) = (Aeson.Object $ x <> y)
unsafeMergeObjects (Aeson.Object x) Aeson.Null = Aeson.Object x
unsafeMergeObjects Aeson.Null (Aeson.Object x) = Aeson.Object x
unsafeMergeObjects Aeson.Null Aeson.Null = Aeson.Null
unsafeMergeObjects x y = Prelude.error $  "unexpected " <> "\n" <> show x  <> "\n" <> show y


class EmptyZohoStructure a where
  emptyZohoStructure :: a

  default emptyZohoStructure :: (Generic a, GEmptyZohoStructure (Rep a)) => a
  emptyZohoStructure = to gEmptyZohoStructure


class GEmptyZohoStructure f where
  gEmptyZohoStructure :: f p

instance (GEmptyZohoStructure f, GEmptyZohoStructure g) => GEmptyZohoStructure (f :*: g) where
  gEmptyZohoStructure = gEmptyZohoStructure :*: gEmptyZohoStructure

instance (GEmptyZohoStructure c) => GEmptyZohoStructure (D1 x c) where
  gEmptyZohoStructure = M1 gEmptyZohoStructure

instance (GEmptyZohoStructure s) => GEmptyZohoStructure (C1 x s) where
  gEmptyZohoStructure = M1 gEmptyZohoStructure

instance (EmptyZohoStructure t) => GEmptyZohoStructure (S1 m (Rec0 t)) where
  gEmptyZohoStructure = M1 (K1 emptyZohoStructure)

instance EmptyZohoStructure (Maybe a) where
  emptyZohoStructure = Nothing

instance EmptyZohoStructure () where
  emptyZohoStructure = ()

instance EmptyZohoStructure Aeson.Value where
  emptyZohoStructure = Aeson.Null

-- omitNothingValues :: Aeson.Value -> Aeson.Value
-- omitNothingValues v = case v of
--   (Aeson.Object) -> _
--   (Aeson.Array a) -> _
--   (Aeson.String _) -> v
--   (Aeson.Number _) -> v
--   (Aeson.Bool _) -> v
--   Aeson.Null -> v

zohoPrefix :: (String -> String)
           -> Aeson.Options
zohoPrefix fn = (Casing.aesonPrefix fn){omitNothingFields=True}
