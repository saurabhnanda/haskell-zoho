module Zoho.Desk.Utils where

import Data.Aeson.Casing as Casing
import Data.Aeson as Aeson
import Data.Char as Char
import Zoho.Types (zohoPrefix)

accountJsonOptions :: Aeson.Options
accountJsonOptions = zohoPrefix $ \s ->
  case s of
    "AnnualRevenue" -> "annualrevenue"
    "ZohoCRMAccount" -> "zohoCRMAccount"
    "AssociatedSLAIds" -> "associatedSLAIds"
    "CustomFields" -> "cf"
    x -> Casing.camelCase x

contactJsonOptions :: Aeson.Options
contactJsonOptions = zohoPrefix $ \s ->
  case s of
    "ZohoCRMAccount" -> "zohoCRMAccount"
    "CustomFields" -> "cf"
    "PhotoUrl" -> "photoURL"
    "Type" -> "Typ"
    x -> Casing.camelCase x
