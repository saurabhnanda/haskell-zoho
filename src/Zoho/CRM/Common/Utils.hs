module Zoho.CRM.Common.Utils where

import Data.Aeson.Casing as Casing
import Data.Aeson as Aeson
import Data.Char as Char
import Zoho.Types (zohoPrefix)

pascalSnakeCase :: String -> String
pascalSnakeCase s = go False s
  where
    go _ [] = []
    go isPrevLower (x:xs) = if Char.isLower x
                            then x:(go True xs)
                            else if isPrevLower
                                 then '_':x:(go False xs)
                                 else x:(go False xs)

googleAdsJsonOptions :: Aeson.Options
googleAdsJsonOptions = zohoPrefix $ \s ->
  case s of
    "AdGroupName" -> "AdGroup_Name"
    "CostPerClick" -> "Cost_per_Click"
    "CostPerConversion" -> "Cost_per_Conversion"
    "ReasonForConversionFailure" -> "Reason_for_Conversion_Failure"
    x -> pascalSnakeCase x
