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

-- ==========================================================================================
-- This file contains the final implementation for the declareLostItem workflow
--
--
-- There are two parts:
-- * the first section contains the (type-only) definitions for each step
-- * the second section contains the implementations for each step
--   and the implementation of the overall workflow
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
  (AttributeCode, CategoryId, CategoryType) -> Either AttributeValidationError (AttributeCode, CategoryId, CategoryType) 

-- ----------------------------------------------------------------------------
-- Validated LostItem
-- ----------------------------------------------------------------------------

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
      vuserId   :: UserId
    , vcontact  :: ValidatedContactInformation
    , vname     :: FullName
    } deriving (Eq, Ord, Show)

data ValidatedContactInformation = ValidatedContactInformation {
      -- Tel required, email optional
      vemail         :: EmailAddress
    , vaddress       :: PostalAddress
    , vprimaryTel    :: Telephone
    , vsecondaryTel  :: Telephone
    } deriving (Eq, Ord, Show)

data ValidatedLostItem = ValidatedLostItem {
      vlostItemId          :: LostItemId
  ,   vlostItemName        :: ItemName
  ,   vlostItemCategoryId  :: CategoryId
  ,   vlostItemDesc        :: LongDescription
  ,   vlostItemLocation    :: ValidatedLocation
  ,   vlostItemLostDate    :: UTCTime
  ,   vlostItemAttributes  :: Set ValidatedAttribute
  ,   vlostItemOwner       :: ValidatedPerson
  }

type ValidateUnvalidatedLostItem =
  CheckAdministrativeAreaInfoValid            -- Dependency
  -> CheckContactInfoValid                    -- Dependency
  -> CheckAttributeInfoValid                  -- Dependancy
  -> UnvalidatedLostItem                      -- Input
  -> UTCTime                                  -- Input
  -> String                                   -- Input
  -> Either ValidationError ValidatedLostItem




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
  assignUuid = 

    do id <- toLostItemId assignUuid
       name <- toLostItemName $ uliName unvalidatedLostItem
       catId <- toCategoryId $ uliCategoryId unvalidatedLostItem
       descpt <- toLostItemDescription $ uliDescription unvalidatedLostItem
       location <- toLostItemLocation checkAdministrativeAreaInfoValid $ ulocation unvalidatedLostItem
       attributes <- sequence $ fmap (toValidatedAttribute checkAttributeInfoValid) $ uliattributes unvalidatedLostItem
       owner <- toOwner checkContactInfoValid $ uowner unvalidatedLostItem
       return ValidatedLostItem {
                    vlostItemId = id
                ,   vlostItemName = name
                ,   vlostItemCategoryId = catId
                ,   vlostItemDesc = descpt
                ,   vlostItemLocation = location
                ,   vlostItemLostDate = decalrationTime 
                ,   vlostItemAttributes = fromList attributes
                ,   vlostItemOwner = owner
                }


  
--- Helper functions for valodateUnvalidatedLostItem
toOwner :: CheckContactInfoValid -> UnvalidatedPerson -> Either ValidationError ValidatedPerson
toOwner checkContactInfoValid uperson =
  do id <- toUserId $ uuserId uperson
     contact <- toContactInfo checkContactInfoValid $ ucontact uperson
     name <- toFullName $ ufullname uperson
     let validPerson = ValidatedPerson {
          vuserId = id
        , vcontact = contact
        , vname = name
        }
     return validPerson
      
toContactInfo :: 
  CheckContactInfoValid 
    -> UnvalidatedContactInformation 
    -> Either ValidationError ValidatedContactInformation 
toContactInfo checkContactInfoValid ucinfo =
  do email <- toEmail $ uemail ucinfo
     address <- toPostalAddress $ uaddress ucinfo
     primaryTel <- toCheckedValidTelephone checkContactInfoValid $ uprimaryTel ucinfo
     secondaryTel <- toCheckedValidTelephone checkContactInfoValid $ usecondaryTel ucinfo
     let validatedContactInformation = 
            ValidatedContactInformation {
                vemail = email
              , vaddress = address   
              , vprimaryTel = primaryTel  
              , vsecondaryTel = secondaryTel
            }
     return validatedContactInformation

toCheckedValidTelephone :: 
  CheckContactInfoValid 
  -> String 
  -> Either ValidationError Telephone
toCheckedValidTelephone checkContactInfoValid str = 
  do tel <- toTelephone str
     validTelephone <- mapLeft ValidationError $ checkContactInfoValid tel
     return validTelephone

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
  do f <- toFirst $ ufirst uFullName
     m <- toMiddle $ umiddle uFullName
     l <- toLast $ ulast uFullName
     let validFullName = FullName {
          first = f
        , middle =  m
        , last = l
        }
     return validFullName

toMiddle :: String -> Either ValidationError (Maybe Middle)
toMiddle str = 
  mapLeft ValidationError $ createMiddle str

toLast :: String -> Either ValidationError LastName
toLast str = 
  mapLeft ValidationError $ createLastName str

toValidatedAttribute :: 
  CheckAttributeInfoValid 
  -> UnvalidatedAttribute
  -> Either ValidationError ValidatedAttribute
toValidatedAttribute checkAttributeInfoValid uattr  =
  do  ( validAttCode, 
       validAttrCatId, 
       validAttrCatType) <- toCheckedAttributeInfo 
                              (uattrCode uattr,  
                              urelatedCategory uattr, 
                              urelatedCategoryType uattr) 
                              checkAttributeInfoValid
      attrName <- toAttributeName $ uattrName uattr
      attrDescpt <- toAttributeDescpt $ uattrDescription uattr
      attrValue <- toAttributeValue $ uattrValue uattr
      attrUnit <- toAttributeUnit $ uattrUnit uattr
     
      let validAttribute = ValidatedAttribute {
            vattrCode = validAttCode     
          , vattrName = attrName   
          , vattrDescription = attrDescpt 
          , vattrValue = Just attrValue  
          , vattrUnit = Just attrUnit      
          , vrelatedCategory = validAttrCatId   
          , vrelatedCategoryType = validAttrCatType
          }
      return validAttribute
     
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
     valTrio <-  mapLeft ValidationError resultCheck 
     return valTrio

toCheckedAttributeInfo :: 
  (String, String, String)  
    -> CheckAttributeInfoValid
    -> Either ValidationError (AttributeCode, CategoryId, CategoryType) 
toCheckedAttributeInfo (acode, cid, ctype) checkAttributeInfoValid =
  do attrCode <- mapLeft ValidationError $ createAttributeCode acode
     catId <- mapLeft ValidationError $ createCategoryId cid
     catType <- mapLeft ValidationError $ toCategoryType ctype
     let resultCheck = checkAttributeInfoValid (attrCode, catId, catType)
     valTrio <-  mapLeft ValidationError resultCheck 
     return valTrio
                  
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
createLostItem validatedLostItem =
        DeclaredLostItem {
            lostItemId = vlostItemId validatedLostItem
        ,   lostItemName = vlostItemName validatedLostItem
        ,   lostItemCategoryId = vlostItemCategoryId validatedLostItem
        ,   lostItemDesc = vlostItemDesc validatedLostItem
        ,   lostItemLocation = toLocation $ vlostItemLocation validatedLostItem
        ,   lostItemLostDate = vlostItemLostDate validatedLostItem
        ,   lostItemAttributes = fromList $ fmap toAttribute $ toList $ vlostItemAttributes validatedLostItem
        ,   lostItemOwner = toPerson $ vlostItemOwner validatedLostItem
        }

--- Helper functions

toPerson :: ValidatedPerson -> Person
toPerson valPerson = 
  Person {
    userId = vuserId valPerson
  , contact = toContactInformation $ vcontact valPerson
  , name = vname valPerson
  }

toContactInformation :: ValidatedContactInformation -> ContactInformation
toContactInformation valContactInfo = 
  ContactInformation {
      email = vemail valContactInfo  
    , address = vaddress valContactInfo
    , primaryTel = vprimaryTel valContactInfo
    , secondaryTel = vsecondaryTel valContactInfo
    }


toAttribute :: ValidatedAttribute -> Attribute
toAttribute valAttr = 
  Attribute {
      attrCode = vattrCode valAttr     
    , attrName = vattrName valAttr     
    , attrDescription = vattrDescription valAttr
    , attrValue = vattrValue valAttr  
    , attrUnit = vattrUnit valAttr    
    , relatedCategory = vrelatedCategory valAttr
    , relatedCategoryType = vrelatedCategoryType valAttr
    }


toLocation :: ValidatedLocation -> Location
toLocation valLocatiion = 
  Location {
        region = vregion valLocatiion
    ,   division = vdivision valLocatiion
    ,   subdivision = vsubdivision valLocatiion
    ,   city = vcity valLocatiion
    ,   village = vvillage valLocatiion
    ,   neighborhood = vneighborhood valLocatiion
    ,   locationAddress = vlocationAddress valLocatiion
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

createLostItemDeclaredEvent :: DeclaredLostItem -> DeclaredLostItem
createLostItemDeclaredEvent declaredLostItem = declaredLostItem

createSearchableLostItemDeclaredEvent :: DeclaredLostItem -> DeclaredLostItem
createSearchableLostItemDeclaredEvent declaredLostItem = declaredLostItem





-- ---------------------------------------------------------------------------- ---
-- ---------------------------------------------------------------------------- ---
                         -- Overall workflow --
-- ---------------------------------------------------------------------------- ---
-- ---------------------------------------------------------------------------- ---

declareLostItem 
  checkAdministrativeAreaInfoValid  -- Dependency
  checkAttributeInfoValid           -- Dependency
  checkContactInfoValid             -- Dependency
  createDeclarationAcknowledgment   -- Dependency
  sendAcknowledgment                -- Dependency
  unvalidatedLostItem               -- Input
  lostItemCreationTime              -- Input
  lostItemUuid =                    -- Input
      do  
          -- Validation step
          validatedLostItem 
              <- validateUnvalidatedLostItem
                    checkAdministrativeAreaInfoValid
                    checkContactInfoValid
                    checkAttributeInfoValid
                    unvalidatedLostItem
                    lostItemCreationTime
                    lostItemUuid

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
          events 
            <- return 
                $ createEvents 
                    createdLostItem
                    maybeAcknowledgment

          return events
          







