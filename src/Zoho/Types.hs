{-# LANGUAGE ScopedTypeVariables #-}
module Zoho.Types where

import Data.Aeson
import GHC.TypeLits
import Data.Proxy
import Data.String.Conv
import Data.Aeson.Casing

data ResponseWrapper (s :: Symbol) a = ResponseWrapper { unwrapResponse :: a } deriving (Eq, Show)

instance (FromJSON a, KnownSymbol s) => FromJSON (ResponseWrapper s a) where
  parseJSON = withObject "Expecting Object to parse into a ResponseWrapper"$ \o -> do
    r <- o .: (toS $ symbolVal (Proxy :: Proxy s))
    pure $ ResponseWrapper r


moduleJsonFieldNameMapping :: String -> String
moduleJsonFieldNameMapping s = case s of
  "EmailTemplateSupport" -> "emailTemplate_support"
  x -> snakeCase x
