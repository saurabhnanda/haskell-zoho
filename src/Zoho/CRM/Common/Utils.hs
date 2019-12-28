module Zoho.CRM.Common.Utils where

import Data.Aeson.Casing as Casing
import Data.Aeson as Aeson

googleAdsJsonOptions :: Aeson.Options
googleAdsJsonOptions = Casing.aesonPrefix $ \s ->
  case s of
    "AdGroupName" -> "AdGroup_Name"
    "CostPerClick" -> "Cost_per_Click"
    "CostPerConversion" -> "Cost_per_Conversion"
    "ReasonForConversionFailure" -> "Reason_for_Conversion_Failure"
    x -> x
