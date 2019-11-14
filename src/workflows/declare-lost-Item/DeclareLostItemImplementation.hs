module DeclareLostItemImplementation where

import CommonCompoundTypes
import CommonSimpleTypes
import Data.Either.Combinators 
    (mapLeft)
import Data.Maybe 
    (maybeToList)
import Data.Set 
    (Set, toList, fromList)
import Data.Time 
    (UTCTime)
import DeclareLostItemPublicTypes
import Util 
    (singleton)




-- ==========================================================================================
-- This file contains the initial implementation for the declareLostItem workflow
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





--- Adminitrative data (Region, Division and Subdivison) validation
---
---




type AdminAreaValidationError = String




type CheckAdministrativeAreaInfoValid =
  (String, String, String) ->
  Either AdminAreaValidationError (Maybe (Region, Division, SubDivision))




type ContactInfoValidationError = String




type CheckContactInfoValid =
  Telephone ->
  Either ContactInfoValidationError Telephone




type AttributeValidationError = String




type CheckAttributeInfoValid =
  UnvalidatedAttribute ->
  UnvalidatedLostItem ->
  Either AttributeValidationError ValidatedAttribute




data ValidatedLocation
  = ValidatedLocation
      { vadminArea :: Maybe AdministrativeAreaInfo,
        vcityOrVillage :: Maybe CityOrVillage,
        vneighborhood :: Maybe Neighborhood,
        vlocationAddresses :: [Address]
      }
  deriving (Eq, Ord, Show)




data ValidatedAttribute
  = ValidatedAttribute
      { vattributeCode :: AttributeCode,
        vattributeName :: AttributeName,
        vattributeDescription :: ShortDescription,
        vattributeValue :: Maybe AttributeValue,
        vattributeUnit :: Maybe AttributeUnit
      }
  deriving (Eq, Ord, Show)




data ValidatedPerson
  = ValidatedPerson
      { vpersonId :: UserId,
        vpersonContact :: ValidatedContactInformation,
        vpersonFullName :: FullName
      }
  deriving (Eq, Ord, Show)




data ValidatedContactInformation
  = ValidatedContactInformation
      { vcontactInfoAddress :: Maybe PostalAddress,
        vcontactInfoMethod :: ContactMethod
      }
  deriving (Eq, Ord, Show)




data ValidatedLostItem
  = ValidatedLostItem
      { validatedLostItemId :: LostItemId,
        validatedLostItemName :: ItemName,
        validatedLostItemCategoryId :: CategoryId,
        validatedLostItemDescription :: LongDescription,
        validatedLostItemLocations :: Set ValidatedLocation,
        validatedLostItemRegistrTime :: UTCTime,
        validatedLostItemDateTimeSpan :: DateTimeSpan,
        validatedLostItemAttributes :: Set ValidatedAttribute,
        validatedLostItemOwner :: ValidatedPerson
      }






-- ----------------------------------------------------------------------------
-- Creation step (see DeclareLostItem type in common modules)
-- ----------------------------------------------------------------------------





-- ----------------------------------------------------------------------------
-- Acknowledgement step
-- ----------------------------------------------------------------------------




newtype HtmlString
  = HtmlString String




data DeclarationAcknowledgment
  = DeclarationAcknowledgment
      { ownerEmail :: ContactInformation,
        letter :: HtmlString
      }




type CreateDeclarationAcknowledgment =
  DeclaredLostItem -> HtmlString




-- Send a lost item declaration / registration acknoledgment to the declarant
-- Note that this does not generate an Either type
-- because on faillure, we will continue anyway.
-- On success, we will generate a DeclarationAcknowledgmentSent event

data SendResult
  = Sent
  | NotSent




type SendAcknowledgment =
  DeclarationAcknowledgment -> SendResult




type AcknowledgemenDeclaredLostItem =
  CreateDeclarationAcknowledgment -> -- Dependency
  SendAcknowledgment -> -- Dependency
  DeclaredLostItem -> -- Input
  Maybe DeclarationAcknowledgmentSent





-- ==========================================================================================
-- Section 2 : Implementation
-- ==========================================================================================




-- ----------------------------------------------------------------------------
-- Validation step
-- ----------------------------------------------------------------------------




validateUnvalidatedLostItem :: 
    CheckAdministrativeAreaInfoValid -- Dependency
    -> CheckContactInfoValid -- Dependency
    -> CheckAttributeInfoValid -- Dependancy
    -> UnvalidatedLostItem -- Input
    -> UTCTime -- Input
    -> UnvalidatedLostItemId -- Input
    -> Either ValidationError ValidatedLostItem -- Output
validateUnvalidatedLostItem
  checkAdministrativeAreaInfoValid
  checkContactInfoValid
  checkAttributeInfoValid
  unvalidatedLostItem
  decalrationTime
  unvalidatedAssignUuid =
    ValidatedLostItem
      <$> itemId 
      <*> name 
      <*> catgrId 
      <*> descpt 
      <*> locts 
      <*> registTime 
      <*> dateTimeSpan 
      <*> attrs 
      <*> owner
    where   itemId = 
                toLostItemId 
                    unvalidatedAssignUuid
            name = 
                toLostItemName 
                . uliName 
                $ unvalidatedLostItem
            catgrId = 
                toCategoryId 
                . uliCategoryId 
                $ unvalidatedLostItem
            descpt = 
                toLongDescpt 
                . uliDescription 
                $ unvalidatedLostItem
            locts = 
                fmap fromList 
                . traverse (toLostItemLocation checkAdministrativeAreaInfoValid) 
                . ulocations
                $ unvalidatedLostItem
            registTime = 
                pure decalrationTime
            dateTimeSpan = 
                toDateTimeSpan 
                . uliDateAndTimeSpan 
                $ unvalidatedLostItem
            attrs = 
                fmap fromList 
                . traverse (toValidatedAttribute checkAttributeInfoValid unvalidatedLostItem) 
                . uliattributes
                $ unvalidatedLostItem
            owner = 
                toOwner checkContactInfoValid 
                . uowner
                $ unvalidatedLostItem
                




--- Helper functions for valodateUnvalidatedLostItem
---
---






toOwner :: CheckContactInfoValid -> UnvalidatedPerson -> Either ValidationError ValidatedPerson
toOwner checkContactInfoValid uperson =
  ValidatedPerson
    <$> (toUserId . uuserId) uperson
    <*> (toContactInfo checkContactInfoValid . ucontact) uperson
    <*> (toFullName . ufullname) uperson




toContactInfo ::
  CheckContactInfoValid ->
  UnvalidatedContactInformation ->
  Either ValidationError ValidatedContactInformation
toContactInfo checkContactInfoValid uc
  -- no email but both prim and sec phone given
  | null givenEmail
      && notNull givenPrimTel
      && notNull givenSecTel =
    do
      adress <- mapValidationError $ crtOptPstAddrss givenAddress
      primTel <- mapValidationError $ crtTel givenPrimTel
      secTel <- mapValidationError $ crtOptTel givenSecTel
      let contactMethod = PhoneOnly primTel secTel
      return ValidatedContactInformation
        { vcontactInfoAddress = adress,
          vcontactInfoMethod = contactMethod
        }
  -- no email but only prim phone given
  | null givenEmail
      && notNull givenPrimTel
      && null givenSecTel =
    do
      adress <- mapValidationError $ crtOptPstAddrss givenAddress
      primTel <- mapValidationError $ crtTel givenPrimTel
      let contactMethod = PhoneOnly primTel Nothing
      return ValidatedContactInformation
        { vcontactInfoAddress = adress,
          vcontactInfoMethod = contactMethod
        }
  -- just email given
  | notNull givenEmail
      && null givenPrimTel
      && null givenSecTel =
    do
      adress <- mapValidationError $ crtOptPstAddrss givenAddress
      email <- mapValidationError $ crtEmailAddress givenEmail
      let contactMethod = EmailOnly email
      return ValidatedContactInformation
        { vcontactInfoAddress = adress,
          vcontactInfoMethod = contactMethod
        }
  -- email and prim phone given
  | notNull givenEmail
      && notNull givenPrimTel
      && null givenSecTel =
    do
      adress <- mapValidationError $ crtOptPstAddrss givenAddress
      primTel <- mapValidationError $ crtTel givenPrimTel
      email <- mapValidationError $ crtEmailAddress givenEmail
      let contactMethod = EmailAndPhone BothContactInfo
            { bothContactInfoEmail = email,
              bothContactInfoPrimTel = primTel,
              bothContactInfoSndTel = Nothing
            }
      return ValidatedContactInformation
        { vcontactInfoAddress = adress,
          vcontactInfoMethod = contactMethod
        }
  -- email, prim and sec phones given
  | notNull givenEmail
      && notNull givenPrimTel
      && notNull givenSecTel =
    do
      adress <- mapValidationError $ crtOptPstAddrss givenAddress
      primTel <- mapValidationError $ crtTel givenPrimTel
      email <- mapValidationError $ crtEmailAddress givenEmail
      secTel <- mapValidationError $ crtOptTel givenSecTel
      let contactMethod = EmailAndPhone BothContactInfo
            { bothContactInfoEmail = email,
              bothContactInfoPrimTel = primTel,
              bothContactInfoSndTel = secTel
            }
      return ValidatedContactInformation
        { vcontactInfoAddress = adress,
          vcontactInfoMethod = contactMethod
        }
  | otherwise = Left $ ValidationError "Provide at least one contact method (Phone or Email)"
  where
    givenEmail = uemail uc
    givenPrimTel = uprimaryTel uc
    givenSecTel = usecondaryTel uc
    givenAddress = uaddress uc




toCheckedValidTelephone ::
  CheckContactInfoValid ->
  String ->
  Either ValidationError Telephone
toCheckedValidTelephone checkContactInfoValid str =
  do
    tel <- toTelephone str
    mapValidationError $ checkContactInfoValid tel




toFullName :: UnvalidatedFullName -> Either ValidationError FullName
toFullName uFullName =
  FullName
    <$> (toFirstName . ufirst) uFullName
    <*> (toMiddleName . umiddle) uFullName
    <*> (toLastName . ulast) uFullName




toValidatedAttribute ::
  CheckAttributeInfoValid ->
  UnvalidatedLostItem ->
  UnvalidatedAttribute ->
  Either ValidationError ValidatedAttribute
toValidatedAttribute checkAttributeInfoValid ulostitem uattr =
  mapValidationError $ checkAttributeInfoValid uattr ulostitem




toCheckedValidAdminArea ::
  (String, String, String) ->
  CheckAdministrativeAreaInfoValid ->
  Either ValidationError (Maybe (Region, Division, SubDivision))
toCheckedValidAdminArea (reg, divs, sub) checkAdministrativeAreaInfoValid =
  mapValidationError $ checkAdministrativeAreaInfoValid (reg, divs, sub)





toLostItemLocation ::
  CheckAdministrativeAreaInfoValid ->
  UnvalidatedLocation ->
  Either ValidationError ValidatedLocation
toLostItemLocation checkAdministrativeAreaInfoValid u =
  do
    adminArea <-
      toCheckedValidAdminArea
        (uadminArea u)
        checkAdministrativeAreaInfoValid
    cityOrVillage <-
      toCityOrVillage (ucity u, uvillage u)
    neighborhood <-
      toNeighborhood $ uneighborhood u
    addresses <-
      traverse toAddress $ uloaddresses u
    return ValidatedLocation
      { vadminArea = adminArea,
        vcityOrVillage = cityOrVillage,
        vneighborhood = neighborhood,
        vlocationAddresses = addresses
      }





-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------





creatteLostItem :: ValidatedLostItem -> DeclaredLostItem
creatteLostItem =
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
  Attribute
    { attributeCode = vattributeCode valAttr,
      attributeName = vattributeName valAttr,
      attributeDescription = vattributeDescription valAttr,
      attributeValue = vattributeValue valAttr,
      attributeUnit = vattributeUnit valAttr
    }




toLocation :: ValidatedLocation -> Location
toLocation vLoc =
  Location
    { locationAdminArea = vadminArea vLoc,
      locationCityOrVillage = vcityOrVillage vLoc,
      locationNeighborhood = vneighborhood vLoc,
      locationAddresses = vlocationAddresses vLoc
    }





-- ----------------------------------------------------------------------------
-- Check refered category enabled step
-- ----------------------------------------------------------------------------





checkRefCatgrEnabled :: 
    ValidatedLostItem 
    -> Category 
    -> Either DomainError ValidatedLostItem
checkRefCatgrEnabled vli (RootCategory refCatgr)
  | validatedLostItemCategoryId vli == categoryId refCatgr =
    case categoryEnablementStatus refCatgr of
      Disabled reason ->
        Left . DomainError $
          "the referenced category is disabled for the following reason: "
            <> reason
      Enabled _ -> return vli
  | otherwise = Left . DomainError $ "category ids don't match"
checkRefCatgrEnabled vli (SubCategory refCatgr _)
  | validatedLostItemCategoryId vli == categoryId refCatgr =
    case categoryEnablementStatus refCatgr of
      Disabled reason ->
        Left . DomainError $
          "the referenced category is disabled for the following reason: "
            <> reason
      Enabled _ -> return vli
  | otherwise = Left . DomainError $ "category ids don't match"






-- ----------------------------------------------------------------------------
-- Aknowledgment step
-- ----------------------------------------------------------------------------






acknowledgemenDeclaredLostItem :: AcknowledgemenDeclaredLostItem
acknowledgemenDeclaredLostItem
  crtDeclarationAcknowledgment
  sendAcknowledgment
  declaredLostItem =
    let letter = crtDeclarationAcknowledgment declaredLostItem
        acknoledgment = DeclarationAcknowledgment
          { ownerEmail = personContact $ lostItemOwner declaredLostItem,
            letter = letter
          }
        resultSent = sendAcknowledgment acknoledgment
     in case resultSent of
          Sent ->
            let event = DeclarationAcknowledgmentSent
                  { declaredLostItemId = lostItemId declaredLostItem,
                    ownerContactInfo = personContact $ lostItemOwner declaredLostItem
                  }
             in Just event
          NotSent ->
            Nothing





-- ----------------------------------------------------------------------------
-- Create Events step
-- ----------------------------------------------------------------------------





createEvents :: 
    DeclaredLostItem 
    -> Maybe DeclarationAcknowledgmentSent 
    -> [DeclareLostItemEvent]
createEvents declaredLosItem optionDeclarationAcknowledgmentSent =
  let acknoledgmentEvents =
        maybeToList 
            $ AcknowledgmentSent 
            <$> optionDeclarationAcknowledgmentSent
      lostDeclrationCreatedEvents =
        singleton 
            $ LostItemDeclared 
            $ crtLostItemDeclaredEvent declaredLosItem
      loctsAddedEvents = 
        singleton 
            . LocationsAdded 
            . crtLoctsAddedEvent 
            $ declaredLosItem
      attrbtsAddedEvents = 
        singleton 
            . AttributesAdded 
            . crtAttrbtesAddedEvent 
            $ declaredLosItem
      searchableItemDeclaredEvents =
        singleton 
            $ SearchableItemDeclared 
            $ crtSearchableLostItemDeclaredEvent declaredLosItem
   in case (head loctsAddedEvents, head attrbtsAddedEvents) of
        (LocationsAdded [], AttributesAdded []) ->
          concat 
            [lostDeclrationCreatedEvents
            , searchableItemDeclaredEvents
            , acknoledgmentEvents
            ]
        (LocationsAdded (x : xs), AttributesAdded []) ->
          concat 
            [lostDeclrationCreatedEvents
            , loctsAddedEvents
            , searchableItemDeclaredEvents
            , acknoledgmentEvents
            ]
        (LocationsAdded [], AttributesAdded (x : xs)) ->
          concat 
            [lostDeclrationCreatedEvents
            , attrbtsAddedEvents
            , searchableItemDeclaredEvents
            , acknoledgmentEvents
            ]
        (LocationsAdded (x : xs), AttributesAdded (y : ys)) ->
          concat 
            [lostDeclrationCreatedEvents
            , loctsAddedEvents
            , attrbtsAddedEvents
            , searchableItemDeclaredEvents
            , acknoledgmentEvents
            ]





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
  CheckAdministrativeAreaInfoValid ->
  CheckAttributeInfoValid ->
  CheckContactInfoValid ->
  CreateDeclarationAcknowledgment ->
  SendAcknowledgment ->
  Category ->
  UnvalidatedLostItem ->
  UTCTime ->
  UnvalidatedLostItemId ->
  Either WorkflowError [DeclareLostItemEvent]
declareLostItem
  checkAdministrativeAreaInfoValid -- Dependency
  checkAttributeInfoValid -- Dependency
  checkContactInfoValid -- Dependency
  crtDeclarationAcknowledgment -- Dependency
  sendAcknowledgment -- Dependency
  referencedCategory -- Input
  unvalidatedLostItem -- Input
  lostItemCreationTime -- Input
  unValidatedlostItemUuid =
    -- Input
    do
      -- Validation step - making sure all field constraints are met

      validatedLostItem <-
        mapLeft Validation $
          validateUnvalidatedLostItem
            checkAdministrativeAreaInfoValid
            checkContactInfoValid
            checkAttributeInfoValid
            unvalidatedLostItem
            lostItemCreationTime
            unValidatedlostItemUuid

      -- Verified referenced category enablement status is enabled step

      valiatedCheckedLostItem <-
        mapLeft Domain $
          checkRefCatgrEnabled
            validatedLostItem
            referencedCategory

      -- Creation step

      crtdLostItem <-
        return $
          creatteLostItem
            valiatedCheckedLostItem

      -- Aknowledgment step

      maybeAcknowledgment <-
        return $
          acknowledgemenDeclaredLostItem
            crtDeclarationAcknowledgment
            sendAcknowledgment
            crtdLostItem

      -- Events creation step

      return $
        createEvents
          crtdLostItem
          maybeAcknowledgment
