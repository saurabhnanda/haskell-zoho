module Zoho.CRM.Common.Utils where

import Data.Aeson.Casing as Casing
import Data.Aeson as Aeson
import Data.Char as Char
import Zoho.Types (zohoPrefix, pascalSnakeCase)


googleAdsJsonOptions :: Aeson.Options
googleAdsJsonOptions = zohoPrefix $ \s ->
  case s of
    "AdGroupName" -> "AdGroup_Name"
    "CostPerClick" -> "Cost_per_Click"
    "CostPerConversion" -> "Cost_per_Conversion"
    "ReasonForConversionFailure" -> "Reason_for_Conversion_Failure"
    x -> pascalSnakeCase x

callJsonOptions :: Aeson.Options
callJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = \s -> case s of
      "callTyp" -> "Call_Type"
      x -> pascalSnakeCase x
  , Aeson.omitNothingFields = True
  }
