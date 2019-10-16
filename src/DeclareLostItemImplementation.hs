module DeclareLostItemImplementation where

import CommonSimpleTypes
import CommonCompoundTypes
import DeclaredLostItemPublicTypes


import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set hiding (singleton)
import Util
import Data.Either.Combinators

import Data.UUID.V4
import Data.UUID  -- Internal

-- ==========================================================================================
-- This file contains the initial implementation for the declareLostItem workflow
--
--
-- There are two parts:
-- * the first section contains the (type-only) definitions for each step
-- * the second section contains the implementations for each step
--   and then the implementation of the overall workflow
-- ==========================================================================================


-- ==========================================================================================
-- Section 1 : Defining each step in the workflow using types
-- ==========================================================================================




-- ----------------------------------------------------------------------------
-- Validation step
-- ----------------------------------------------------------------------------




-- Adminitrative data (Region, Division and Subdivison) validation

type AdminAreaValidationError = String

type CheckAdministrativeAreaInfoValid = 
  (Region, Division, SubDivision) 
    -> Either AdminAreaValidationError (Region, Division, SubDivision)

-- Contact Information (Phone number) validation

type ContactInfoValidationError = String


type CheckContactInfoValid = 
  Telephone 
    -> Either ContactInfoValidationError Telephone 


-- Attribute Information (Are they consistent with the category they reference ?) validation

type AttributeValidationError = String
    

type CheckAttributeInfoValid = 
  UnvalidatedAttribute 
    -> UnvalidatedLostItem 
    -> Either AttributeValidationError ValidatedAttribute


-- Validated LostItem

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
    , vrelatedCategories    :: [(CategoryId, CategoryType)]
    } deriving (Eq, Ord, Show)

data ValidatedPerson = ValidatedPerson {
    -- Revoir si l'attribut userId est optionel
      vuserId   :: UserId
    , vcontact  :: ValidatedContactInformation
    , vname     :: FullName
    } deriving (Eq, Ord, Show)

data ValidatedContactInformation = ValidatedContactInformation {
      -- Maybe Tel required, Email optional ????
      vemail         :: EmailAddress
    , vaddress       :: PostalAddress
    , vprimaryTel    :: Telephone
    , vsecondaryTel  :: Telephone
    } deriving (Eq, Ord, Show)

data ValidatedLostItem = ValidatedLostItem {
      vlostItemId               :: LostItemId
  ,   vlostItemName             :: ItemName
  ,   vlostItemCategoryId       :: CategoryId
  ,   vlostItemDesc             :: LongDescription
  ,   vlostItemLocation         :: Set ValidatedLocation
  ,   vlostItemRegistrationTime :: UTCTime
  ,   vlostItemDateAndTimeSpan  :: DateTimeSpan
  ,   vlostItemAttributes       :: Set ValidatedAttribute
  ,   vlostItemOwner            :: ValidatedPerson
  }

type ValidateUnvalidatedLostItem =
  CheckAdministrativeAreaInfoValid            -- Dependency
  -> CheckContactInfoValid                    -- Dependency
  -> CheckAttributeInfoValid                  -- Dependancy
  -> UnvalidatedLostItem                      -- Input
  -> UTCTime                                  -- Input
  -> UnvalidatedLostItemId                    -- Input
  -> Either ValidationError ValidatedLostItem -- Output




-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------




type CreateLostItem =
  ValidatedLostItem -> DeclaredLostItem




-- ----------------------------------------------------------------------------
-- Acknowledgement step
-- ----------------------------------------------------------------------------



newtype HtmlString = 
  HtmlString String

data DeclarationAcknowledgment = DeclarationAcknowledgment {
    ownerEmail :: ContactInformation
  , letter :: HtmlString
  }

type CreateDeclarationAcknowledgment = 
  DeclaredLostItem -> HtmlString

-- Send a lost item declaration / registration acknoledgment to the declarant
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




-- ----------------------------------------------------------------------------
-- Create events step
-- ----------------------------------------------------------------------------



type CreateEvents = 
  DeclaredLostItem 
  -> Maybe DeclarationAcknowledgmentSent 
  -> [DeclareLostItemEvent]












-- ==========================================================================================
-- Section 2 : Implementation
-- ==========================================================================================


-- ----------------------------------------------------------------------------
-- Validation step
-- ----------------------------------------------------------------------------



validateUnvalidatedLostItem :: ValidateUnvalidatedLostItem
validateUnvalidatedLostItem 
  checkAdministrativeAreaInfoValid 
  checkContactInfoValid 
  checkAttributeInfoValid 
  unvalidatedLostItem 
  decalrationTime
  unvalidatedAssignUuid = 

  -- Check out record wild card extention
  -- Use hlint
    ValidatedLostItem
      <$> toLostItemId unvalidatedAssignUuid
      <*> (toLostItemName . uliName) unvalidatedLostItem
      <*> (toCategoryId . uliCategoryId) unvalidatedLostItem
      <*> (toLostItemDescription . uliDescription) unvalidatedLostItem
      <*> (fmap fromList . traverse (toLostItemLocation checkAdministrativeAreaInfoValid) . ulocations) unvalidatedLostItem
      <*> pure decalrationTime
      <*> (toDateTimeSpan . uliDateAndTimeSpan) unvalidatedLostItem
      <*> (fmap fromList . traverse (toValidatedAttribute checkAttributeInfoValid unvalidatedLostItem) . uliattributes) unvalidatedLostItem
      <*> (toOwner checkContactInfoValid . uowner) unvalidatedLostItem


--- Helper functions for valodateUnvalidatedLostItem


toDateTimeSpan :: (String, String) -> Either ValidationError DateTimeSpan
toDateTimeSpan (startDate, endDate) = 
  mapLeft ValidationError $ creatDateTimeSpan startDate endDate " "

toOwner :: CheckContactInfoValid -> UnvalidatedPerson -> Either ValidationError ValidatedPerson
toOwner checkContactInfoValid uperson =
  ValidatedPerson 
    <$> (toUserId . uuserId) uperson
    <*> (toContactInfo checkContactInfoValid . ucontact) uperson
    <*> (toFullName . ufullname) uperson

    
toContactInfo :: 
  CheckContactInfoValid 
    -> UnvalidatedContactInformation 
    -> Either ValidationError ValidatedContactInformation 
toContactInfo checkContactInfoValid ucinfo =
  ValidatedContactInformation 
    <$> (toEmail . uemail) ucinfo
    <*> (toPostalAddress . uaddress) ucinfo
    <*> (toCheckedValidTelephone checkContactInfoValid . uprimaryTel) ucinfo
    <*> (toCheckedValidTelephone checkContactInfoValid . usecondaryTel) ucinfo

toCheckedValidTelephone :: 
  CheckContactInfoValid 
  -> String 
  -> Either ValidationError Telephone
toCheckedValidTelephone checkContactInfoValid str = 
  do tel <- toTelephone str
     mapLeft ValidationError $ checkContactInfoValid tel

toTelephone :: String -> Either ValidationError Telephone
toTelephone str = 
  mapLeft ValidationError $ createTelephone str
    
toEmail :: String -> Either ValidationError EmailAddress
toEmail str = 
  mapLeft ValidationError $ createEmailAddress str
 
toPostalAddress :: String -> Either ValidationError PostalAddress
toPostalAddress str = 
  mapLeft ValidationError $ createPostalAddress str
    
toFirst :: String -> Either ValidationError FirstName
toFirst str = 
  mapLeft ValidationError $ createFirstName str

toFullName :: UnvalidatedFullName -> Either ValidationError FullName
toFullName uFullName =
  FullName 
    <$> (toFirst . ufirst) uFullName
    <*> (toMiddle . umiddle) uFullName
    <*> (toLast . ulast) uFullName

  

toMiddle :: String -> Either ValidationError (Maybe Middle)
toMiddle str = 
  mapLeft ValidationError $ createMiddle str

toLast :: String -> Either ValidationError LastName
toLast str = 
  mapLeft ValidationError $ createLastName str

toValidatedAttribute :: 
  CheckAttributeInfoValid 
  -> UnvalidatedLostItem
  -> UnvalidatedAttribute
  -> Either ValidationError ValidatedAttribute
toValidatedAttribute 
  checkAttributeInfoValid ulostitem uattr  =
  mapLeft ValidationError $ checkAttributeInfoValid uattr ulostitem 
      
     
toLostItemId :: String -> Either ValidationError LostItemId
toLostItemId str = 
  mapLeft ValidationError $ createLostItemId str     

toLostItemName :: String -> Either ValidationError ItemName
toLostItemName str = 
    mapLeft ValidationError $ createItemName str     

toCategoryId :: String -> Either ValidationError CategoryId
toCategoryId str = 
  mapLeft ValidationError $ createCategoryId str    

toUserId :: String -> Either ValidationError UserId
toUserId str = 
  mapLeft ValidationError $ createUserId str    

toLostItemDescription :: String -> Either ValidationError LongDescription
toLostItemDescription str = 
  mapLeft ValidationError $ createLongDescription str     

toCheckedValidAdminArea :: 
  (String, String, String)  
    -> CheckAdministrativeAreaInfoValid
    -> Either ValidationError (Region, Division, SubDivision) 
toCheckedValidAdminArea (r, d, s) checkAdministrativeAreaInfoValid =
  do reg <- mapLeft ValidationError $ toRegion r
     div <- mapLeft ValidationError $ toDivision d
     sub <- mapLeft ValidationError $ toSubDivision s
     let resultCheck = checkAdministrativeAreaInfoValid (reg, div, sub)
     mapLeft ValidationError resultCheck 
               
toCity :: String -> Either ValidationError City
toCity str = 
  mapLeft ValidationError $ createCity str

toVillage :: String -> Either ValidationError Village
toVillage str = 
  mapLeft ValidationError $ createVillage str

toNeighborhood :: String -> Either ValidationError Neighborhood
toNeighborhood str = 
  mapLeft ValidationError $ createNeighborhood str

toAddress :: String -> Either ValidationError Address
toAddress str = 
  mapLeft ValidationError $ createAddress str

toAttributeName :: String -> Either ValidationError AttributeName
toAttributeName str = 
  mapLeft ValidationError $ createAttributeName str

toAttributeDescpt :: String -> Either ValidationError ShortDescription
toAttributeDescpt str = 
  mapLeft ValidationError $ createShortDescription str

toAttributeValue :: String -> Either ValidationError AttributeValue
toAttributeValue str = 
  mapLeft ValidationError $ createAttributeValue str

toAttributeUnit :: String -> Either ValidationError AttributeUnit
toAttributeUnit str = 
  mapLeft ValidationError $ createAttributeUnit str

toLostItemLocation ::  
  CheckAdministrativeAreaInfoValid 
    -> UnvalidatedLocation 
    -> Either ValidationError ValidatedLocation
toLostItemLocation checkAdministrativeAreaInfoValid u =
  do 
    (region, 
     division, 
     subDivison) <- toCheckedValidAdminArea
                      (uregion u, udivision u, usubdivision u ) 
                      checkAdministrativeAreaInfoValid
    city <- toCity $ ucity u
    village <- toVillage $ uvillage u
    neighborhood <- toNeighborhood $ uregion u
    address <- toAddress $ uregion u
    let validateLocation = 
          ValidatedLocation {
                vregion = region
            ,   vdivision = division
            ,   vsubdivision = subDivison
            ,   vcity = city
            ,   vvillage = village
            ,   vneighborhood = neighborhood
            ,   vlocationAddress = address
            }
    return validateLocation 




-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------


createLostItem :: CreateLostItem
createLostItem  =
  DeclaredLostItem 
    <$> vlostItemId
    <*> vlostItemName
    <*> vlostItemCategoryId
    <*> vlostItemDesc
    <*> fromList . fmap toLocation . toList . vlostItemLocation
    <*> vlostItemDateAndTimeSpan
    <*> vlostItemRegistrationTime
    <*> fromList . fmap toAttribute . toList . vlostItemAttributes
    <*> toPerson . vlostItemOwner



--- Helper functions

toPerson :: ValidatedPerson -> Person
toPerson = 
  Person 
    <$> vuserId
    <*> toContactInformation . vcontact
    <*> vname
  

toContactInformation :: ValidatedContactInformation -> ContactInformation
toContactInformation = 
  ContactInformation 
    <$> vemail
    <*> vaddress
    <*> vprimaryTel
    <*> vsecondaryTel



toAttribute :: ValidatedAttribute -> Attribute
toAttribute valAttr = 
  Attribute {
      attrCode = vattrCode valAttr     
    , attrName = vattrName valAttr     
    , attrDescription = vattrDescription valAttr
    , attrValue = vattrValue valAttr  
    , attrUnit = vattrUnit valAttr    
    , relatedCategories = vrelatedCategories valAttr
    }


toLocation :: ValidatedLocation -> Location
toLocation = 
  Location 
    <$> vregion
    <*> vdivision
    <*> vsubdivision
    <*> vcity
    <*> vvillage
    <*> vneighborhood
    <*> vlocationAddress






-- ----------------------------------------------------------------------------
-- Aknowledgment step
-- ----------------------------------------------------------------------------


acknowledgemenDeclaredLostItem :: AcknowledgemenDeclaredLostItem
acknowledgemenDeclaredLostItem 
  createDeclarationAcknowledgment
  sendAcknowledgment
  declaredLostItem = 

  let letter = createDeclarationAcknowledgment declaredLostItem
      acknoledgment = DeclarationAcknowledgment {
           ownerEmail =  contact $ lostItemOwner declaredLostItem
         , letter = letter
         }
      resultSent = sendAcknowledgment acknoledgment
  in case resultSent of
      Sent -> 
        let event = DeclarationAcknowledgmentSent {
              declaredLostItemId = lostItemId declaredLostItem
            , ownerContactInfo = contact $ lostItemOwner declaredLostItem
            }
        in Just event
      NotSent ->
        Nothing







-- ----------------------------------------------------------------------------
-- Create Events step
-- ----------------------------------------------------------------------------



createEvents :: CreateEvents
createEvents declaredLosItem optionDeclarationAcknowledgmentSent =
  let acknoledgmentEvents = 
        maybeToList $ fmap AcknowledgmentSent optionDeclarationAcknowledgmentSent
      lostDeclrationCreatedEvents = 
        singleton $ LostItemDeclared $ createLostItemDeclaredEvent declaredLosItem
      searchableItemDeclaredEvents = 
        singleton $ SearchableItemDeclared $ createSearchableLostItemDeclaredEvent declaredLosItem
  in  concat [acknoledgmentEvents, lostDeclrationCreatedEvents, searchableItemDeclaredEvents]



--- Helper functions 

createLostItemDeclaredEvent :: DeclaredLostItem -> DeclaredLostItem
createLostItemDeclaredEvent declaredLostItem = declaredLostItem

createSearchableLostItemDeclaredEvent :: DeclaredLostItem -> DeclaredLostItem
createSearchableLostItemDeclaredEvent declaredLostItem = declaredLostItem





-- ---------------------------------------------------------------------------- ---
-- ---------------------------------------------------------------------------- ---
                         -- Overall workflow --
-- ---------------------------------------------------------------------------- ---
-- ---------------------------------------------------------------------------- ---
declareLostItem ::
  CheckAdministrativeAreaInfoValid
  -> CheckAttributeInfoValid
  -> CheckContactInfoValid
  -> CreateDeclarationAcknowledgment
  -> SendAcknowledgment
  -> UnvalidatedLostItem
  -> UTCTime
  -> UnvalidatedLostItemId
  -> Either DeclareLostItemError [DeclareLostItemEvent]
declareLostItem 
  checkAdministrativeAreaInfoValid  -- Dependency
  checkAttributeInfoValid           -- Dependency
  checkContactInfoValid             -- Dependency
  createDeclarationAcknowledgment   -- Dependency
  sendAcknowledgment                -- Dependency
  unvalidatedLostItem               -- Input
  lostItemCreationTime              -- Input
  unValidatedlostItemUuid =                    -- Input
      do  
          -- Validation step
          validatedLostItem 
              <- mapLeft 
                  Validation $
                    validateUnvalidatedLostItem
                      checkAdministrativeAreaInfoValid
                      checkContactInfoValid
                      checkAttributeInfoValid
                      unvalidatedLostItem
                      lostItemCreationTime
                      unValidatedlostItemUuid

          -- Creation step
          createdLostItem 
              <- return 
                  $ createLostItem validatedLostItem

          -- Aknowledgment step
          maybeAcknowledgment 
            <- return 
                $ acknowledgemenDeclaredLostItem
                    createDeclarationAcknowledgment
                    sendAcknowledgment
                    createdLostItem

          -- Events creation step
          return 
            $ createEvents 
                createdLostItem
                maybeAcknowledgment   

          



--- Check out ThinkPad Lenonvo
--- Check out ThinkPad Lenonvo
--- Check out ThinkPad Lenonvo
--- Check out ThinkPad Lenonvo
--- Check out ThinkPad Lenonvo
--- Check out ThinkPad Lenonvo
--- Check out ThinkPad Lenonvo


--- pramp.com 






