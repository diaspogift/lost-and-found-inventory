module DeclareLostItemImplementation where

import CommonSimpleTypes
import CommonCompoundTypes
import DeclaredLostItemPublicTypes


import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set hiding (singleton, null)
import Util
import Data.Either.Combinators

import Data.UUID.V4
import Data.UUID hiding (null) -- Internal

-- import Data.List.NonEmpty



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



--- Dependencies types
---
---


--- Adminitrative data (Region, Division and Subdivison) validation

type AdminAreaValidationError = String

type CheckAdministrativeAreaInfoValid = 
  (String, String, String) 
    -> Either AdminAreaValidationError (Maybe (Region, Division, SubDivision))


--- Contact Information (Phone number) validation (Probably with an external service???)


type ContactInfoValidationError = String

type CheckContactInfoValid = 
  Telephone 
    -> Either ContactInfoValidationError Telephone 


--- Attribute Information (Are they consistent with the category they reference ?) validation


type AttributeValidationError = String
    

type CheckAttributeInfoValid = 
  UnvalidatedAttribute 
    -> UnvalidatedLostItem 
    -> Either AttributeValidationError ValidatedAttribute




--- Validated LostItem

data ValidatedLocation = ValidatedLocation {
        vadminArea :: Maybe AdministrativeAreaInfo
    ,   vcityOrVillage :: Maybe CityOrVillage
    ,   vneighborhood :: Maybe Neighborhood
    ,   vlocationAddresses :: [Address]
    } deriving (Eq, Ord, Show)

data ValidatedAttribute = ValidatedAttribute {
      vattrCode             :: AttributeCode
    , vattrName             :: AttributeName
    , vattrDescription      :: ShortDescription
    , vattrValue            :: Maybe AttributeValue
    , vattrUnit             :: Maybe AttributeUnit
    } deriving (Eq, Ord, Show)

data ValidatedPerson = ValidatedPerson {
      vuserId   :: UserId
    , vcontact  :: ValidatedContactInformation
    , vname     :: FullName
    } deriving (Eq, Ord, Show)

data ValidatedContactInformation = ValidatedContactInformation {
      vaddress       :: PostalAddress
    , vContactMethod    :: ContactMethod
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
toContactInfo checkContactInfoValid uc
    -- no email but both prim and sec phone given
    | null givenEmail 
      && (not . null) givenPrimTel 
      && (not . null) givenSecTel =
        do  adress <- mapLeft ValidationError $ createPostalAddress givenAddress
            primTel <- mapLeft ValidationError $ createTelephone givenPrimTel
            secTel <- mapLeft ValidationError $ createOptionalTelephone givenSecTel

            let contactMethod = PhoneOnly primTel secTel
            return  ValidatedContactInformation {
                        vaddress = adress
                    ,   vContactMethod = contactMethod
                    }

    -- no email but only prim phone given
    | null givenEmail
      && (not . null) givenPrimTel 
      && null givenSecTel =
        do  adress <- mapLeft ValidationError $ createPostalAddress givenAddress
            primTel <- mapLeft ValidationError $ createTelephone givenPrimTel
            let contactMethod = PhoneOnly primTel Nothing
            return  ValidatedContactInformation {
                        vaddress = adress
                    ,   vContactMethod = contactMethod
                    }
    
    -- just email given
    | (not. null) givenEmail
      && null givenPrimTel 
      && null givenSecTel =
        do  adress <- mapLeft ValidationError $ createPostalAddress givenAddress
            email <- mapLeft ValidationError $ createEmailAddress givenEmail
            let contactMethod = EmailOnly email
            return  ValidatedContactInformation {
                        vaddress = adress
                    ,   vContactMethod = contactMethod
                    }
            
    -- email and prim phone given
    | (not . null) givenEmail
      && (not . null) givenPrimTel
      && null givenSecTel =
        do  adress <- mapLeft ValidationError $ createPostalAddress givenAddress
            primTel <- mapLeft ValidationError $ createTelephone givenPrimTel
            email <- mapLeft ValidationError $ createEmailAddress givenEmail
            let contactMethod = EmailAndPhone BothContactInfo {
                    emailInfo = email
                ,   primTelephoneInfo = primTel
                ,   secTelephoneInfo = Nothing
                }
            return  ValidatedContactInformation {
                        vaddress = adress
                    ,   vContactMethod = contactMethod
                    }

    -- email, prim and sec phones given
    | (not . null) givenEmail 
      && (not . null)  givenPrimTel  
      && (not . null) givenSecTel =
        do  adress <- mapLeft ValidationError $ createPostalAddress givenAddress
            primTel <- mapLeft ValidationError $ createTelephone givenPrimTel
            email <- mapLeft ValidationError $ createEmailAddress givenEmail
            secTel <- mapLeft ValidationError $ createOptionalTelephone givenSecTel

            let contactMethod = EmailAndPhone BothContactInfo {
                    emailInfo = email
                ,   primTelephoneInfo = primTel
                ,   secTelephoneInfo = secTel
                }

            return  ValidatedContactInformation {
                        vaddress = adress
                    ,   vContactMethod = contactMethod
                    }

    | otherwise = error "Invalid contact information"

    where   givenEmail = uemail uc
            givenPrimTel = uprimaryTel uc
            givenSecTel = usecondaryTel uc
            givenAddress = uaddress uc

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
    -> Either ValidationError (Maybe (Region, Division, SubDivision))
toCheckedValidAdminArea (reg, div, sub) checkAdministrativeAreaInfoValid =
    do  resultCheck 
            <- mapLeft 
                ValidationError $
                checkAdministrativeAreaInfoValid (reg, div, sub)
        return resultCheck 
        
toCityOrVillage :: 
    (String, String) 
    -> Either ValidationError (Maybe CityOrVillage)
toCityOrVillage (cityStr, villageStr)
    | null cityStr && null villageStr = 
        return Nothing
    | null cityStr && (not . null) villageStr = 
        do  village <- mapLeft ValidationError $ createVillage villageStr
            return $ Just $ Country village
    | (not . null) cityStr && null villageStr = 
        do  city <- mapLeft ValidationError $ createCity cityStr
            return $ Just $ Urban city
    | (not . null) cityStr && (not . null) villageStr = 
        Left $ ValidationError "provide either a city or a village not both"
    | otherwise = return Nothing


toCity :: String -> Either ValidationError City
toCity str = 
  mapLeft ValidationError $ createCity str

toVillage :: String -> Either ValidationError Village
toVillage str = 
  mapLeft ValidationError $ createVillage str

toNeighborhood :: String -> Either ValidationError (Maybe Neighborhood)
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

toAttributeValue :: String -> Either ValidationError (Maybe AttributeValue)
toAttributeValue str = 
  mapLeft ValidationError $ createOptionalAttributeValue str

toAttributeUnit :: String -> Either ValidationError (Maybe AttributeUnit)
toAttributeUnit str = 
  mapLeft ValidationError $ createOptionalAttributeUnit str

toLostItemLocation ::  
  CheckAdministrativeAreaInfoValid 
    -> UnvalidatedLocation 
    -> Either ValidationError ValidatedLocation
toLostItemLocation checkAdministrativeAreaInfoValid u =
  do    adminArea 
            <- toCheckedValidAdminArea
                    (uadminArea u)
                    checkAdministrativeAreaInfoValid
        cityOrVillage
            <- toCityOrVillage (ucity u, uvillage u)
        neighborhood 
            <- toNeighborhood $ uneighborhood u
        addresses 
            <- traverse toAddress $ uloaddresses u
                
        return  ValidatedLocation {
                        vadminArea = adminArea
                    ,   vcityOrVillage = cityOrVillage
                    ,   vneighborhood = neighborhood
                    ,   vlocationAddresses = addresses
                    }
          




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
---
---


toPerson :: ValidatedPerson -> Person
toPerson = 
  Person 
    <$> vuserId
    <*> toContactInformation . vcontact
    <*> vname
  

toContactInformation :: ValidatedContactInformation -> ContactInformation
toContactInformation = 
  ContactInformation 
    <$> vaddress
    <*> vContactMethod


toAttribute :: ValidatedAttribute -> Attribute
toAttribute valAttr = 
  Attribute {
      attrCode = vattrCode valAttr     
    , attrName = vattrName valAttr     
    , attrDescription = vattrDescription valAttr
    , attrValue = vattrValue valAttr  
    , attrUnit = vattrUnit valAttr    
    }


toLocation :: ValidatedLocation -> Location
toLocation vLoc =
    Location {
        adminArea = vadminArea vLoc
    ,   cityOrVillage = vcityOrVillage vLoc
    ,   neighborhood = vneighborhood vLoc
    ,   locationAddresses = vlocationAddresses vLoc
    }
    





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
---
---

createLostItemDeclaredEvent :: DeclaredLostItem -> DeclaredLostItem
createLostItemDeclaredEvent declaredLostItem = declaredLostItem

createSearchableLostItemDeclaredEvent :: DeclaredLostItem -> DeclaredLostItem
createSearchableLostItemDeclaredEvent declaredLostItem = declaredLostItem





-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --
                         -- Overall workflow --
-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --





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
  unValidatedlostItemUuid =         -- Input
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






