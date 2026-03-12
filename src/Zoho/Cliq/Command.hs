{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Types for Zoho Cliq slash command suggestion handlers.
--
-- Suggestion handlers return a list of 'CliqSuggestion' (max 50).
-- When the user selects a suggestion, the full map (including 'csId')
-- is passed back in the execution handler's @selections@ array.
module Zoho.Cliq.Command
  ( CliqSuggestion(..)
  ) where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Text (Text)
import GHC.Generics

-- | A single entry in a slash command suggestion list.
--
-- Cliq displays suggestions as the user types after the slash command.
-- Only 'csTitle' is required; all other fields are optional.
--
-- Serializes to: @{\"title\":\"...\",\"description\":\"...\",\"imageurl\":\"...\",\"id\":\"...\"}@
data CliqSuggestion = CliqSuggestion
  { csTitle :: !Text              -- ^ Main display text
  , csDescription :: !(Maybe Text)  -- ^ Secondary text below title
  , csImageurl :: !(Maybe Text)     -- ^ Icon\/image URL displayed alongside
  , csId :: !(Maybe Text)           -- ^ Identifier passed back in @selections@ when picked
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSuggestion where
  toJSON = genericToJSON (aesonPrefix snakeCase) { omitNothingFields = True }
