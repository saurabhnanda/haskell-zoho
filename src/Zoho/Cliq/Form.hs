{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Zoho.Cliq.Form
  ( -- * Form Fields
    FormField(..)
  , SelectOption(..)
  , SelectOptionGroup(..)
  , DateFilter(..)
  , DateTimeValue(..)
  , PhoneValue(..)
  , PhoneFilter(..)

    -- * Form Error Response
  , CliqFormError(..)
  ) where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.Types (camelTo2)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics

-- ============================================================================
-- Supporting types
-- ============================================================================

-- | A single option in a select/radio/dynamic_select field
data SelectOption = SelectOption
  { soLabel :: !Text
  , soValue :: !Text
  , soThumbnail :: !(Maybe Text)
  , soDescription :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON SelectOption where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) { omitNothingFields = True }

-- | Grouped options for select fields (alternative to flat list)
data SelectOptionGroup = SelectOptionGroup
  { sogLabel :: !Text
  , sogOptions :: ![SelectOption]
  } deriving (Eq, Show, Generic)

instance ToJSON SelectOptionGroup where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) { omitNothingFields = True }

-- | Date/datetime filter constraints
data DateFilter = DateFilter
  { dfFrom :: !(Maybe Text)
  , dfTo :: !(Maybe Text)
  , dfAllowedDays :: !(Maybe [Text])
  } deriving (Eq, Show, Generic)

instance ToJSON DateFilter where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) { omitNothingFields = True }

-- | Default value for datetime fields
data DateTimeValue = DateTimeValue
  { dtvDateTime :: !Text
  , dtvTimeZoneId :: !Text
  } deriving (Eq, Show, Generic)

instance ToJSON DateTimeValue where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) { omitNothingFields = True }

-- | Default value for phone_number fields
data PhoneValue = PhoneValue
  { pvPhoneNumber :: !Text
  , pvCountryCode :: !Text        -- ^ ISO alpha-3 lowercase ("ind", "usa")
  } deriving (Eq, Show, Generic)

instance ToJSON PhoneValue where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) { omitNothingFields = True }

-- | Phone number filter (restrict available country codes)
data PhoneFilter = PhoneFilter
  { pfCountryCode :: ![Text]
  } deriving (Eq, Show, Generic)

instance ToJSON PhoneFilter where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) { omitNothingFields = True }

-- ============================================================================
-- Form Field (13 variants, discriminated by "type")
-- ============================================================================

-- | Form field — 13 variants discriminated by @"type"@.
--
-- Uses 'TaggedObject' sum encoding so each variant serializes as a flat object
-- with a @"type"@ field injected automatically.
--
-- Fields that appear in multiple constructors must have the same Haskell type
-- (GHC requirement with DuplicateRecordFields). Therefore polymorphic fields
-- use 'Value':
--
-- * @fieldValue@ — Text for most, Bool for toggle, object for datetime/phone.
--   Use 'toJSON' to wrap: @fieldValue = Just (toJSON True)@,
--   @fieldValue = Just (toJSON myDateTimeValue)@
-- * Filter fields use per-variant names (@dateFilter@, @datetimeFilter@,
--   @phoneFilter@, @nativeFilter@) with typed values. The @aesonPrefix@
--   codec strips each variant prefix, so they all serialize to @"filter"@.
-- * @fieldOptions@ — @[SelectOption]@; use @[]@ when options are not applicable.
--
-- Example: @FormFieldText { fieldName = "email", fieldLabel = "Email", ... }@
-- serializes to @{"type": "text", "name": "email", "label": "Email", ...}@
data FormField
  = FormFieldText
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldPlaceholder :: !Text
    , fieldHint :: !(Maybe Text)
    , fieldValue :: !(Maybe Value)
    , fieldMandatory :: !(Maybe Bool)
    , fieldDisabled :: !(Maybe Bool)
    , fieldTriggerOnChange :: !(Maybe Bool)
    , fieldMaxLength :: !(Maybe Text)
    , fieldMinLength :: !(Maybe Text)
    , fieldFormat :: !(Maybe Text)           -- ^ "email", "url", "password"
    }
  | FormFieldTextarea
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldPlaceholder :: !Text
    , fieldHint :: !(Maybe Text)
    , fieldValue :: !(Maybe Value)
    , fieldMandatory :: !(Maybe Bool)
    , fieldDisabled :: !(Maybe Bool)
    , fieldMaxLength :: !(Maybe Text)
    , fieldMinLength :: !(Maybe Text)
    }
  | FormFieldNumber
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldPlaceholder :: !Text
    , fieldHint :: !(Maybe Text)
    , fieldValue :: !(Maybe Value)
    , fieldMandatory :: !(Maybe Bool)
    , fieldMin :: !(Maybe Text)
    , fieldMax :: !(Maybe Text)
    , fieldStepValue :: !(Maybe Int)
    }
  | FormFieldDate
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldPlaceholder :: !Text
    , fieldValue :: !(Maybe Value)           -- ^ Text "yyyy-MM-DD" wrapped in toJSON
    , fieldMandatory :: !(Maybe Bool)
    , dateFilter :: !(Maybe DateFilter)
    }
  | FormFieldDatetime
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldPlaceholder :: !Text
    , fieldValue :: !(Maybe Value)           -- ^ DateTimeValue wrapped in toJSON
    , fieldMandatory :: !(Maybe Bool)
    , datetimeFilter :: !(Maybe DateFilter)
    }
  | FormFieldSelect
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldPlaceholder :: !Text
    , fieldHint :: !(Maybe Text)
    , fieldValue :: !(Maybe Value)
    , fieldMandatory :: !(Maybe Bool)
    , fieldMultiple :: !(Maybe Bool)
    , fieldMaxSelections :: !(Maybe Int)
    , fieldTriggerOnChange :: !(Maybe Bool)
    , fieldOptions :: ![SelectOption]
    }
  | FormFieldDynamicSelect
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldPlaceholder :: !Text
    , fieldMandatory :: !(Maybe Bool)
    , fieldMultiple :: !(Maybe Bool)
    , fieldMaxSelections :: !(Maybe Int)
    , fieldRefreshCutoff :: !(Maybe Int)
    , fieldOptions :: ![SelectOption]
    }
  | FormFieldNativeSelect
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldPlaceholder :: !Text
    , fieldDataSource :: !Text               -- ^ "contacts", "channels", "conversations", "teams"
    , fieldMultiple :: !(Maybe Bool)
    , fieldMaxSelections :: !(Maybe Int)
    , fieldValue :: !(Maybe Value)
    , nativeFilter :: !(Maybe Value)         -- ^ varies by data_source
    }
  | FormFieldRadio
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldHint :: !(Maybe Text)
    , fieldValue :: !(Maybe Value)
    , fieldMandatory :: !(Maybe Bool)
    , fieldTriggerOnChange :: !(Maybe Bool)
    , fieldOptions :: ![SelectOption]
    }
  | FormFieldToggle
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldHint :: !(Maybe Text)
    , fieldValue :: !(Maybe Value)           -- ^ Bool wrapped in toJSON
    , fieldTriggerOnChange :: !(Maybe Bool)
    , fieldDisabled :: !(Maybe Bool)
    }
  | FormFieldPhoneNumber
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldPlaceholder :: !Text
    , fieldMandatory :: !(Maybe Bool)
    , fieldValue :: !(Maybe Value)           -- ^ PhoneValue wrapped in toJSON
    , phoneFilter :: !(Maybe PhoneFilter)
    }
  | FormFieldFile
    { fieldName :: !Text
    , fieldLabel :: !Text
    , fieldPlaceholder :: !Text
    , fieldMandatory :: !(Maybe Bool)
    , fieldMultiple :: !(Maybe Bool)
    , fieldMaxSelections :: !(Maybe Int)
    , fieldMaxSize :: !(Maybe Text)          -- ^ in KB, max 51200
    }
  | FormFieldHidden
    { fieldName :: !Text
    , fieldValue :: !(Maybe Value)
    }
  deriving (Eq, Show, Generic)

formFieldJsonOptions :: Options
formFieldJsonOptions = (aesonPrefix snakeCase)
  { omitNothingFields = True
  , sumEncoding = TaggedObject { tagFieldName = "type", contentsFieldName = "contents" }
  , constructorTagModifier = camelTo2 '_' . drop 9  -- drop "FormField"
  }

instance ToJSON FormField where
  toJSON = genericToJSON formFieldJsonOptions

-- ============================================================================
-- Form Error Response
-- ============================================================================

-- | Validation error response from a form submit handler.
--
-- Return this to reject form submission and show field-level errors inline.
-- Cliq keeps the form open and renders each error next to the matching field.
--
-- The @cfeInputs@ keys must match the @fieldName@ values from the form definition.
--
-- Serializes to:
-- @{"type": "form_error", "text": "Please fix errors", "inputs": {"phone": "Invalid number"}}@
data CliqFormError = CliqFormError
  { cfeText :: !Text                         -- ^ top-level error banner
  , cfeInputs :: !(HashMap Text Text)        -- ^ fieldName -> error message
  } deriving (Eq, Show, Generic)

instance ToJSON CliqFormError where
  toJSON CliqFormError{..} = object
    [ "type" .= ("form_error" :: Text)
    , "text" .= cfeText
    , "inputs" .= cfeInputs
    ]
