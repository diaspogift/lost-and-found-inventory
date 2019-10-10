module DeclareLostItemImplementation where

import CommonSimpleTypes
import CommonCompoundTypes
import DeclaredLostItemPublicTypes


import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set

-- ============================================================================
-- This file contains the final implementation for the declareLostItem workflow
--
--
-- There are two parts:
-- * the first section contains the (type-only) definitions for each step
-- * the second section contains the implementations for each step
--   and the implementation of the overall workflow
-- ============================================================================


-- ============================================================================
-- Section 1 : Define each step in the workflow using types
-- ============================================================================




-- ---------------------------
-- Validation step
-- ---------------------------




-- Adminitrative data (Region, Division and Subdivison) validation

data AdminAreaValidationError = 
    RegionNotFound
  | DivisionNotFound
  | SubDivisionNotFound
  | InconcistentData

type CheckValidAdminArea = 
  (Region, Division, SubDivision) 
    -> Either AdminAreaValidationError (Region, Division, SubDivision)

-- Contact Information (Phone number) validation

data ContactInfoValidationError =
      InvalidPhoneNumber
    | AddressNotFound

type CheckContactInfoValid = 
  UnvalidatedContactInformation 
    -> Either ContactInfoValidationError ValidatedContactInformation 


-- Attribute Information (Are they consistent with the category they reference ?) validation

data AttributeValidationError =
      InvalidRefCategoryType
    | InvalidAttribute
    | InconcistentCategoryType


type CheckAttributeInfoValid = 
  UnvalidatedAttribute -> Either AttributeValidationError ValidatedAttribute 

-- ---------------------------
-- Validated LostItem
-- ---------------------------

data ValidatedLocation = ValidatedLocation {
        vregion :: Region
    ,   vdivision :: Division
    ,   vsubdivision :: SubDivision
    ,   vcity :: City
    ,   vvillage :: Village
    ,   vneighborhood :: Neighborhood
    ,   vlocationAddress :: Address
    } deriving (Eq, Ord, Show)

data ValidatedAttribute = ValidatedAttribute {
      vattrCode             :: AttributeCode
    , vattrName             :: AttributeName
    , vattrDescription      :: ShortDescription
    , vattrValue            :: Maybe AttributeValue
    , vattrUnit             :: Maybe AttributeUnit
    , vrelatedCategory      :: CategoryId
    , vrelatedCategoryType  :: CategoryType
    } deriving (Eq, Ord, Show)

data ValidatedPerson = ValidatedPerson {
    -- Revoir si user est optionelle
      userId   :: UserId
    , contact  :: ValidatedContactInformation
    , name     :: FullName
    } deriving (Eq, Ord, Show)

data ValidatedContactInformation = ValidatedContactInformation {
      -- Tel required, email optional
      email         :: EmailAddress
    , address       :: PostalAddress
    , primaryTel    :: Telephone
    , secondaryTel  :: Telephone
    } deriving (Eq, Ord, Show)

data ValidatedLostItem = ValidatedLostItem {
      vlostItemId          :: LostItemId
  ,   vlostItemName        :: ItemName
  ,   vlostItemCategoryId  :: CategoryId
  ,   vlostItemLocation    :: ValidatedLocation
  ,   vlostItemDesc        :: LongDescription
  ,   vlostItemLostDate    :: UTCTime
  ,   vlostItemAttributes  :: Set ValidatedAttribute
  ,   vlostItemOwner       :: ValidatedPerson
  }

type ValidateLostItem =
  CheckValidAdminArea -- Dependency
  -> CheckContactInfoValid -- Dependency
  -> CheckAttributeInfoValid -- Dependancy
  -> UnvalidatedContactInformation -- Input
  -> Either ValidationError ValidatedLostItem




-- ---------------------------
-- Creation step
-- ---------------------------




type CreateLostItem =
  ValidatedLostItem -> DeclaredLostItem




-- ---------------------------
-- Send Acknowledgement step
-- ---------------------------



newtype HtmlString = 
  HtmlString String

data DeclarationAcknowledgment = DeclarationAcknowledgment {
    ownerEmail :: ContactInformation
  , letter :: HtmlString
  }

type CreateDeclarationAcknowledgment = 
  DeclaredLostItem -> HtmlString

-- Send the lost declaration acknoledgment to the declarant
-- Note that this does not generate an Either type 
-- because on faillure, we will continue anyway.
-- On success, we will generate a DeclarationAcknowledgmentSent event

data SendResult = 
    Sent
  | NotSent
  
type SendAcknowledgment = 
  DeclarationAcknowledgment -> SendResult

type AcknowledgemenDeclaredLostItem = 
  CreateDeclarationAcknowledgment   -- Dependency
  -> SendAcknowledgment             -- Dependency
  -> DeclaredLostItem
  -> Maybe DeclarationAcknowledgmentSent




-- ---------------------------
-- Create events step
-- ---------------------------



type CreateEvents = 
  DeclaredLostItem 
  -> Maybe DeclarationAcknowledgmentSent 
  -> [DeclareLostItemEvent]





