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
      vattributeCode             :: AttributeCode
    , vattributeName             :: AttributeName
    , vattributeDescription      :: ShortDescription
    , vattributeValue            :: Maybe AttributeValue
    , vattributeUnit             :: Maybe AttributeUnit
    } deriving (Eq, Ord, Show)



data ValidatedPerson = ValidatedPerson {
      vpersonId         :: UserId
    , vpersonContact    :: ValidatedContactInformation
    , vpersonFullName   :: FullName
    } deriving (Eq, Ord, Show)



data ValidatedContactInformation = ValidatedContactInformation {
      vcontactInfoAddress       :: Maybe PostalAddress
    , vcontactInfoMethod    :: ContactMethod
    } deriving (Eq, Ord, Show)



data ValidatedLostItem = ValidatedLostItem {
      validatedLostItemId               :: LostItemId
  ,   validatedLostItemName             :: ItemName
  ,   validatedLostItemCategoryId       :: CategoryId
  ,   validatedLostItemDescription      :: LongDescription
  ,   validatedLostItemLocations        :: Set ValidatedLocation
  ,   validatedLostItemRegistrTime      :: UTCTime
  ,   validatedLostItemDateTimeSpan     :: DateTimeSpan
  ,   validatedLostItemAttributes       :: Set ValidatedAttribute
  ,   validatedLostItemOwner            :: ValidatedPerson
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




-- ----------------------------------------------------------------------------
-- Check refered Category enablement status is enabled
-- ----------------------------------------------------------------------------



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

    ValidatedLostItem
      <$> id <*> name <*> catId <*> descpt <*> locts <*> regTime <*> dteTimeSpan <*> attrs <*> owner
    where id = toLostItemId unvalidatedAssignUuid
          name = (toLostItemName . uliName) unvalidatedLostItem
          catId = (toCategoryId . uliCategoryId) unvalidatedLostItem
          descpt = (toLostItemDescription . uliDescription) unvalidatedLostItem
          locts = (fmap fromList . traverse (toLostItemLocation checkAdministrativeAreaInfoValid) . ulocations) unvalidatedLostItem
          regTime = pure decalrationTime
          dteTimeSpan = (toDateTimeSpan . uliDateAndTimeSpan) unvalidatedLostItem
          attrs = (fmap fromList . traverse (toValidatedAttribute checkAttributeInfoValid unvalidatedLostItem) . uliattributes) unvalidatedLostItem
          owner = (toOwner checkContactInfoValid . uowner) unvalidatedLostItem



--- Helper functions for valodateUnvalidatedLostItem

toDateTimeSpan :: (String, String) -> Either ValidationError DateTimeSpan
toDateTimeSpan (startDate, endDate) = 
  mapLeft ValidationError $ crtDtTmSpan startDate endDate " "

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
        do  adress <- mapLeft ValidationError $ crtOptPstAddrss givenAddress
            primTel <- mapLeft ValidationError $ crtTel givenPrimTel
            secTel <- mapLeft ValidationError $ crtOptTel givenSecTel

            let contactMethod = PhoneOnly primTel secTel
            return  ValidatedContactInformation {
                        vcontactInfoAddress = adress
                    ,   vcontactInfoMethod = contactMethod
                    }

    -- no email but only prim phone given
    | null givenEmail
      && (not . null) givenPrimTel 
      && null givenSecTel =
        do  adress <- mapLeft ValidationError $ crtOptPstAddrss givenAddress
            primTel <- mapLeft ValidationError $ crtTel givenPrimTel
            let contactMethod = PhoneOnly primTel Nothing
            return  ValidatedContactInformation {
                        vcontactInfoAddress = adress
                    ,   vcontactInfoMethod = contactMethod
                    }
    
    -- just email given
    | (not. null) givenEmail
      && null givenPrimTel 
      && null givenSecTel =
        do  adress <- mapLeft ValidationError $ crtOptPstAddrss givenAddress
            email <- mapLeft ValidationError $ crtEmailAddress givenEmail
            let contactMethod = EmailOnly email
            return  ValidatedContactInformation {
                        vcontactInfoAddress = adress
                    ,   vcontactInfoMethod = contactMethod
                    }
            
    -- email and prim phone given
    | (not . null) givenEmail
      && (not . null) givenPrimTel
      && null givenSecTel =
        do  adress <- mapLeft ValidationError $ crtOptPstAddrss givenAddress
            primTel <- mapLeft ValidationError $ crtTel givenPrimTel
            email <- mapLeft ValidationError $ crtEmailAddress givenEmail
            let contactMethod = EmailAndPhone BothContactInfo {
                    bothContactInfoEmail    = email
                ,   bothContactInfoPrimTel  = primTel
                ,   bothContactInfoSndTel   = Nothing
                }
            return  ValidatedContactInformation {
                        vcontactInfoAddress = adress
                    ,   vcontactInfoMethod = contactMethod
                    }

    -- email, prim and sec phones given
    | (not . null) givenEmail 
      && (not . null)  givenPrimTel  
      && (not . null) givenSecTel =
        do  adress <- mapLeft ValidationError $ crtOptPstAddrss givenAddress
            primTel <- mapLeft ValidationError $ crtTel givenPrimTel
            email <- mapLeft ValidationError $ crtEmailAddress givenEmail
            secTel <- mapLeft ValidationError $ crtOptTel givenSecTel

            let contactMethod = EmailAndPhone BothContactInfo {
                    bothContactInfoEmail    = email
                ,   bothContactInfoPrimTel  = primTel
                ,   bothContactInfoSndTel   = secTel
                }

            return  ValidatedContactInformation {
                        vcontactInfoAddress = adress
                    ,   vcontactInfoMethod = contactMethod
                    }

    | otherwise = Left $ ValidationError "Provide at least one contact method (Phone or Email)"

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
  mapLeft ValidationError $ crtTel str
    

toEmail :: String -> Either ValidationError EmailAddress
toEmail str = 
  mapLeft ValidationError $ crtEmailAddress str
 

toPostalAddress :: String -> Either ValidationError PostalAddress
toPostalAddress str = 
  mapLeft ValidationError $ crtPstAddress str
    

toFirst :: String -> Either ValidationError FirstName
toFirst str = 
  mapLeft ValidationError $ crtFstNm str

  
toFullName :: UnvalidatedFullName -> Either ValidationError FullName
toFullName uFullName =
  FullName 
    <$> (toFirst . ufirst) uFullName
    <*> (toMiddle . umiddle) uFullName
    <*> (toLast . ulast) uFullName

  
toMiddle :: String -> Either ValidationError (Maybe Middle)
toMiddle str = 
  mapLeft ValidationError $ crtMdleNm str


toLast :: String -> Either ValidationError LastName
toLast str = 
  mapLeft ValidationError $ crtLstNm str


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
  mapLeft ValidationError $ crtLstItmId str     


toLostItemName :: String -> Either ValidationError ItemName
toLostItemName str = 
    mapLeft ValidationError $ crtItmNm str     


toCategoryId :: String -> Either ValidationError CategoryId
toCategoryId str = 
  mapLeft ValidationError $ crtCatgrId str    


toUserId :: String -> Either ValidationError UserId
toUserId str = 
  mapLeft ValidationError $ crtUsrId str    


toLostItemDescription :: String -> Either ValidationError LongDescription
toLostItemDescription str = 
  mapLeft ValidationError $ crtLgDescpt str     


toCheckedValidAdminArea :: 
  (String, String, String)  
    -> CheckAdministrativeAreaInfoValid
    -> Either ValidationError (Maybe (Region, Division, SubDivision))
toCheckedValidAdminArea (reg, div, sub) checkAdministrativeAreaInfoValid =
    mapLeft ValidationError $ checkAdministrativeAreaInfoValid (reg, div, sub)
     
    
toCityOrVillage :: 
    (String, String) 
    -> Either ValidationError (Maybe CityOrVillage)
toCityOrVillage (cityStr, villageStr)
    | null cityStr && null villageStr = 
        return Nothing
    | null cityStr && (not . null) villageStr = 
        do  village <- mapLeft ValidationError $ crtVillage villageStr
            return $ Just $ Country village
    | (not . null) cityStr && null villageStr = 
        do  city <- mapLeft ValidationError $ crtCity cityStr
            return $ Just $ Urban city
    | (not . null) cityStr && (not . null) villageStr = 
        Left $ ValidationError "provide either a city or a village not both"
    | otherwise = return Nothing



toCity :: String -> Either ValidationError City
toCity str = 
  mapLeft ValidationError $ crtCity str


toVillage :: String -> Either ValidationError Village
toVillage str = 
  mapLeft ValidationError $ crtVillage str


toNeighborhood :: String -> Either ValidationError (Maybe Neighborhood)
toNeighborhood str = 
  mapLeft ValidationError $ crtNghbrhd str


toAddress :: String -> Either ValidationError Address
toAddress str = 
  mapLeft ValidationError $ crtAddress str


toAttributeName :: String -> Either ValidationError AttributeName
toAttributeName str = 
  mapLeft ValidationError $ crtAttrNm str


toAttributeDescpt :: String -> Either ValidationError ShortDescription
toAttributeDescpt str = 
  mapLeft ValidationError $ crtShrtDescpt str


toAttributeValue :: String -> Either ValidationError (Maybe AttributeValue)
toAttributeValue str = 
  mapLeft ValidationError $ crtOptAttrVal str


toAttributeUnit :: String -> Either ValidationError (Maybe AttributeUnit)
toAttributeUnit str = 
  mapLeft ValidationError $ crtOptAttrUnt str


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




creatteLostItem :: ValidatedLostItem -> DeclaredLostItem
creatteLostItem  =

    DeclaredLostItem 
        <$> validatedLostItemId 
        <*> validatedLostItemName 
        <*> validatedLostItemCategoryId 
        <*> validatedLostItemDescription 
        <*> fromList 
            . fmap toLocation 
            . toList 
            . validatedLostItemLocations 
        <*> validatedLostItemDateTimeSpan 
        <*> validatedLostItemRegistrTime 
        <*> fromList 
            . fmap toAttribute 
            . toList 
            . validatedLostItemAttributes 
        <*> toPerson . validatedLostItemOwner 

    
      
--- Helper functions
---
---


toPerson :: ValidatedPerson -> Person
toPerson = 
  Person 
    <$> vpersonId
    <*> toContactInformation . vpersonContact
    <*> vpersonFullName
  

toContactInformation :: ValidatedContactInformation -> ContactInformation
toContactInformation = 
  ContactInformation 
    <$> vcontactInfoAddress
    <*> vcontactInfoMethod


toAttribute :: ValidatedAttribute -> Attribute
toAttribute valAttr = 
  Attribute {
      attributeCode = vattributeCode valAttr     
    , attributeName = vattributeName valAttr     
    , attributeDescription = vattributeDescription valAttr
    , attributeValue = vattributeValue valAttr  
    , attributeUnit = vattributeUnit valAttr    
    }


toLocation :: ValidatedLocation -> Location
toLocation vLoc =
    Location {
        locationAdminArea = vadminArea vLoc
    ,   locationCityOrVillage = vcityOrVillage vLoc
    ,   locationNeighborhood = vneighborhood vLoc
    ,   locationAddresses = vlocationAddresses vLoc
    }
    





-- ----------------------------------------------------------------------------
-- Check refered category enabled step
-- ----------------------------------------------------------------------------






checkRefCatgrEnabled :: ValidatedLostItem -> Category-> Either DomainError ValidatedLostItem
checkRefCatgrEnabled vli (RootCategory refCatgr) 
    | validatedLostItemCategoryId vli == categoryId refCatgr = 
        case categoryEnablementStatus refCatgr of
            Disabled reason ->
                Left . DomainError 
                    $ "the referenced category is disabled for the following reason: " 
                    <> reason
            Enabled _ -> return vli

    | otherwise =  Left . DomainError $ "category ids don't match"
checkRefCatgrEnabled vli (SubCategory refCatgr _) 
    | validatedLostItemCategoryId vli == categoryId refCatgr = 
        case categoryEnablementStatus refCatgr of
            Disabled reason ->
                Left . DomainError 
                    $ "the referenced category is disabled for the following reason: " 
                    <> reason
            Enabled _ -> return vli

    | otherwise =  Left . DomainError $ "category ids don't match"


    



-- ----------------------------------------------------------------------------
-- Aknowledgment step
-- ----------------------------------------------------------------------------






acknowledgemenDeclaredLostItem :: AcknowledgemenDeclaredLostItem
acknowledgemenDeclaredLostItem 
  crtDeclarationAcknowledgment
  sendAcknowledgment
  declaredLostItem = 

  let letter = crtDeclarationAcknowledgment declaredLostItem
      acknoledgment = DeclarationAcknowledgment {
           ownerEmail =  personContact $ lostItemOwner declaredLostItem
         , letter = letter
         }
      resultSent = sendAcknowledgment acknoledgment
  in case resultSent of
      Sent -> 
        let event = DeclarationAcknowledgmentSent {
              declaredLostItemId = lostItemId declaredLostItem
            , ownerContactInfo = personContact $ lostItemOwner declaredLostItem
            }
        in Just event
      NotSent ->
        Nothing






-- ----------------------------------------------------------------------------
-- Create Events step
-- ----------------------------------------------------------------------------





createEvents :: DeclaredLostItem -> Maybe DeclarationAcknowledgmentSent -> [DeclareLostItemEvent]
createEvents declaredLosItem optionDeclarationAcknowledgmentSent =
  let acknoledgmentEvents = 
        maybeToList $ AcknowledgmentSent <$> optionDeclarationAcknowledgmentSent

      lostDeclrationCreatedEvents = 
        singleton $ LostItemDeclared $ crtLostItemDeclaredEvent declaredLosItem

      loctsAddedEvents = singleton . LocationsAdded . crtLoctsAddedEvent $ declaredLosItem

      attrbtsAddedEvents = singleton . AttributesAdded . crtAttrbtesAddedEvent $ declaredLosItem

      searchableItemDeclaredEvents = 
        singleton $ SearchableItemDeclared $ crtSearchableLostItemDeclaredEvent declaredLosItem

  in case  (head loctsAddedEvents, head attrbtsAddedEvents) of
        (LocationsAdded [], AttributesAdded []) ->
            concat [lostDeclrationCreatedEvents, searchableItemDeclaredEvents, acknoledgmentEvents]
        (LocationsAdded (x:xs), AttributesAdded []) ->
            concat [lostDeclrationCreatedEvents, loctsAddedEvents, searchableItemDeclaredEvents, acknoledgmentEvents]
        (LocationsAdded [] , AttributesAdded (x:xs)) ->
            concat [lostDeclrationCreatedEvents, attrbtsAddedEvents, searchableItemDeclaredEvents, acknoledgmentEvents]
        (LocationsAdded (x:xs), AttributesAdded (y:ys)) ->
            concat [lostDeclrationCreatedEvents, loctsAddedEvents, attrbtsAddedEvents, searchableItemDeclaredEvents, acknoledgmentEvents]




--- Helper functions 
---
---

crtLostItemDeclaredEvent :: DeclaredLostItem -> DeclaredLostItem
crtLostItemDeclaredEvent declaredLostItem = declaredLostItem

crtSearchableLostItemDeclaredEvent :: DeclaredLostItem -> DeclaredLostItem
crtSearchableLostItemDeclaredEvent declaredLostItem = declaredLostItem

crtLoctsAddedEvent :: DeclaredLostItem -> [Location]
crtLoctsAddedEvent = toList . lostItemLocations 

crtAttrbtesAddedEvent :: DeclaredLostItem -> [Attribute]
crtAttrbtesAddedEvent = toList . lostItemAttributes 



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
  -> Category
  -> UnvalidatedLostItem
  -> UTCTime
  -> UnvalidatedLostItemId
  -> Either WorkflowError [DeclareLostItemEvent]

declareLostItem 
  checkAdministrativeAreaInfoValid  -- Dependency
  checkAttributeInfoValid           -- Dependency
  checkContactInfoValid             -- Dependency
  crtDeclarationAcknowledgment      -- Dependency
  sendAcknowledgment                -- Dependency
  referencedCategory                -- Input
  unvalidatedLostItem               -- Input
  lostItemCreationTime              -- Input
  unValidatedlostItemUuid =         -- Input
      do  
          -- Validation step - making sure all field constraints are met
          validatedLostItem 
              <- mapLeft Validation $
                    validateUnvalidatedLostItem
                      checkAdministrativeAreaInfoValid
                      checkContactInfoValid
                      checkAttributeInfoValid
                      unvalidatedLostItem
                      lostItemCreationTime
                      unValidatedlostItemUuid

          -- Verified referenced category enablement status is enabled step
          valiatedCheckedLostItem 
              <- mapLeft Domain $
                    checkRefCatgrEnabled
                    validatedLostItem
                    referencedCategory

          -- Creation step
          crtdLostItem 
              <- return $
                    creatteLostItem 
                        valiatedCheckedLostItem

          -- Aknowledgment step
          maybeAcknowledgment 
            <- return 
                $ acknowledgemenDeclaredLostItem
                    crtDeclarationAcknowledgment
                    sendAcknowledgment
                    crtdLostItem

          -- Events creation step
          return 
            $ createEvents 
                crtdLostItem
                maybeAcknowledgment   

          





