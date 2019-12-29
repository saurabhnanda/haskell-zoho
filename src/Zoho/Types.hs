{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Zoho.Types where

import Data.Aeson
import GHC.TypeLits
import Data.Proxy
import Data.String.Conv
import Data.Aeson as Aeson
import Data.Text
import Data.Aeson.Casing
import Data.List as DL
import Data.Char as Char

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
