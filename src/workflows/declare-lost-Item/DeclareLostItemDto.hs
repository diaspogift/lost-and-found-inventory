{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}


module DeclareLostItemDto where


import Data.Aeson
import CommonSimpleTypes
import qualified CommonCompoundTypes as Cct
import DeclaredLostItemPublicTypes


import Prelude hiding (last, id)
import Data.Time
import Data.Set hiding (null, singleton)
import Data.Map hiding (null, toList)

import GHC.Generics






-- ==========================================================================================
-- This file contains the the logic for working with data transfer objects (DTOs)
--
-- Each type of DTO is defined using primitive, serializable types and there are 
-- toUnvlidated, toDomain and fromDomain functions defined for each of them.
--
-- ==========================================================================================


-- ==========================================================================================
-- DTOs for DeclareLostItem workflow
-- ==========================================================================================




-- ----------------------------------------------------------------------------
-- DTO for Location
-- ----------------------------------------------------------------------------



data LocationDto = LocationDto {
        region :: String
    ,   division :: String
    ,   subdivision :: String
    ,   city :: String
    ,   village :: String
    ,   neighborhood :: String
    ,   locationAddress :: [String]
    } deriving (Generic, Show)

instance ToJSON LocationDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LocationDto


-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedLocation :: LocationDto -> UnvalidatedLocation
toUnvalidatedLocation dto  =
    UnvalidatedLocation {
            uadminArea = (region dto, division dto, subdivision dto)
        ,   ucity = city dto
        ,   uvillage = village dto
        ,   uneighborhood = neighborhood dto
        ,   uloaddresses = locationAddress dto
        }
        


toLocation :: LocationDto -> Either ErrorMessage Cct.Location
toLocation dto = 
    do  area <- toAdminAreaInfo (region dto, division dto, subdivision dto)
        lieu <- toCityOrVillage (city dto, village dto)
        voisinage <- crtOptNghbrhd $ neighborhood dto
        addresses <- traverse toAddress $ locationAddress dto
        return Cct.Location  {
                    Cct.adminArea = area
                ,   Cct.cityOrVillage = lieu
                ,   Cct.neighborhood = voisinage
                ,   Cct.locationAddresses = addresses
                }


toAdminAreaInfo :: 
    (String, String, String) -> Either ErrorMessage (Maybe Cct.AdministrativeAreaInfo) 
toAdminAreaInfo (strReg, strDiv, strSub)
    | null strReg && null strDiv, null strSub = Right Nothing
    | otherwise = 
        do  reg <- Cct.toRegion strReg
            div <- Cct.toDivision strDiv
            sub <- Cct.toSubDivision strSub
            return $ Just (reg, div, sub)

toCityOrVillage :: 
    (String, String) 
    -> Either ErrorMessage (Maybe CityOrVillage)
toCityOrVillage (strCity, strVillage)
    | isStringNull strCity && isStringNull strVillage = 
        return Nothing
    | isStringNull strCity && isStringNotNull strVillage = 
        do  village <- crtVillage strVillage
            return $ Just $ Country village
    | isStringNotNull strCity && isStringNull strVillage = 
        do  city <- crtCity strCity
            return $ Just $ Urban city
    | otherwise = return Nothing 
    where   isStringNotNull = (not . null)
            isStringNull = null



toAddress :: String -> Either ErrorMessage Address
toAddress = crtAddress 

fromLocation :: Cct.Location -> LocationDto
fromLocation loc = 
    let (reg, div, sub) = fromMaybeAdminArea $ Cct.adminArea loc
        (city, village) = fromMaybeCityOrVillage $ Cct.cityOrVillage loc
        maybeNeighborhood = fromMaybeNeighborhood $ Cct.neighborhood loc
        addresses = fmap uwrpAddress $ Cct.locationAddresses loc
    in LocationDto {
            region = reg
        ,   division = div
        ,   subdivision = sub
        ,   city = city
        ,   village = village
        ,   neighborhood = maybeNeighborhood
        ,   locationAddress = addresses
        }

fromMaybeAdminArea :: Maybe Cct.AdministrativeAreaInfo -> (String, String, String)
fromMaybeAdminArea Nothing = ("", "", "")
fromMaybeAdminArea (Just (reg, div, sub)) = (Cct.fromRegion reg, Cct.fromDivision div, Cct.fromSubDivision sub)

fromMaybeCityOrVillage :: Maybe CityOrVillage -> (String, String)
fromMaybeCityOrVillage (Just (Urban wCity))= (uwrpCity wCity, "")
fromMaybeCityOrVillage (Just (Country wVillage))= ("", uwrpVillage wVillage)
fromMaybeCityOrVillage Nothing = ("","")


fromMaybeNeighborhood :: Maybe Neighborhood -> String
fromMaybeNeighborhood (Just wNeighborhood) = uwrpNghbrhd wNeighborhood
fromMaybeNeighborhood Nothing = ""

-- ----------------------------------------------------------------------------
-- DTO for Attribute
-- ----------------------------------------------------------------------------



data AttributeDto = AttributeDto {
      attrCode             :: String
    , attrName             :: String
    , attrDescription      :: String
    , attrValue            :: String
    , attrUnit             :: String
    } deriving (Generic, Show)

instance ToJSON AttributeDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AttributeDto

-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedAttribute :: AttributeDto -> UnvalidatedAttribute 
toUnvalidatedAttribute = 
    UnvalidatedAttribute 
        <$> attrCode 
        <*> attrName 
        <*> attrDescription 
        <*> attrValue 
        <*> attrUnit 
    

toAttribute :: AttributeDto -> Either ErrorMessage Cct.Attribute
toAttribute dto = 
    do  code <- crtAttrCd $ attrCode dto
        name <- crtAttrNm $ attrName dto
        desc <- crtShrtDescpt $ attrDescription dto
        val <- crtOptAttrVal $ attrValue dto
        unit <- crtOptAttrUn $ attrUnit dto

        return  Cct.Attribute {
                  Cct.attrCode = code
                , Cct.attrName = name
                , Cct.attrDescription = desc
                , Cct.attrValue = val
                , Cct.attrUnit = unit
                }
        where 
            toCatIdCatTypePair (strCatId, strCatType) =
                do  catId <- crtCatgrId strCatId
                    catType <- Cct.toCategoryType strCatType
                    return (catId, catType)

fromAttribute :: Cct.Attribute -> AttributeDto
fromAttribute  = 
    AttributeDto 
        <$> uwrpAttrCd . Cct.attrCode 
        <*> uwrpAttrNm . Cct.attrName 
        <*> uwrpShrtDescpt . Cct.attrDescription 
        <*> uwrpAttrVal . Cct.attrValue 
        <*> uwrpAttrUn . Cct.attrUnit 





-- ----------------------------------------------------------------------------
-- DTO for Person
-- ----------------------------------------------------------------------------

data PersonDto = PersonDto {
        userId :: String
    ,   contact :: ContactInformationDto
    ,   fullname :: FullNameDto
    } deriving (Generic, Show)


instance ToJSON PersonDto where
    toEncoding = genericToEncoding defaultOptions
    
instance FromJSON PersonDto

-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedPerson :: PersonDto -> UnvalidatedPerson
toUnvalidatedPerson = 
    UnvalidatedPerson
        <$> userId 
        <*> toUnvalidatedContactInformation . contact
        <*> toUnvalidatedFullName . fullname 


fromPerson :: Cct.Person -> PersonDto
fromPerson = 
    PersonDto
        <$> uwrpUsrId . Cct.userId 
        <*> fromContactInformation . Cct.contact 
        <*> fromFullName . Cct.name 
    


-- ----------------------------------------------------------------------------
-- DTO for ContactInformation
-- ----------------------------------------------------------------------------


data ContactInformationDto = ContactInformationDto {
        email :: String
    ,   address :: String
    ,   primaryTel :: String
    ,   secondaryTel :: String 
    } deriving (Generic, Show)


instance ToJSON ContactInformationDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ContactInformationDto

-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedContactInformation :: 
    ContactInformationDto -> UnvalidatedContactInformation
toUnvalidatedContactInformation =
    UnvalidatedContactInformation
        <$> email 
        <*> address 
        <*> primaryTel 
        <*> secondaryTel 
    

toContactInformation ::
     ContactInformationDto -> Either ErrorMessage Cct.ContactInformation
toContactInformation dto = 
    do add <- (crtOptPstAddrss . address) dto
       contact <- toContactMethod (email dto, primaryTel dto, secondaryTel dto)
       return Cct.ContactInformation {
                    Cct.address = add
                ,   Cct.contactMethod = contact
                }
        

toContactMethod :: (String, String, String) -> Either ErrorMessage Cct.ContactMethod
toContactMethod (givenEmail, givenPrimTel, givenSecTel)
    -- no email but both prim and sec phone given
    | null givenEmail 
      && (not . null) givenPrimTel 
      && (not . null) givenSecTel =
        do  primTel <- crtTel givenPrimTel
            secTel <- crtOptTel givenSecTel
            return $ Cct.PhoneOnly primTel secTel
          

    -- no email but only prim phone given
    | null givenEmail
      && (not . null) givenPrimTel 
      && null givenSecTel =
        do  primTel <- crtTel givenPrimTel
            return $ Cct.PhoneOnly primTel Nothing
            

    -- just email given
    | (not. null) givenEmail
      && null givenPrimTel 
      && null givenSecTel =
        do  email <- crtEmailAddress givenEmail
            return $ Cct.EmailOnly email
            
    -- email and prim phone given
    | (not . null) givenEmail
      && (not . null) givenPrimTel
      && null givenSecTel =
        do  primTel <- crtTel givenPrimTel
            email <- crtEmailAddress givenEmail
            return $ 
                Cct.EmailAndPhone Cct.BothContactInfo {
                        Cct.emailInfo = email
                    ,   Cct.primTelephoneInfo = primTel
                    ,   Cct.secTelephoneInfo = Nothing
                    }
            
    -- email, prim and sec phones given
    | (not . null) givenEmail 
      && (not . null)  givenPrimTel  
      && (not . null) givenSecTel =
        do  primTel <- crtTel givenPrimTel
            email <- crtEmailAddress givenEmail
            secTel <- crtOptTel givenSecTel
            return $ 
                Cct.EmailAndPhone Cct.BothContactInfo {
                        Cct.emailInfo = email
                    ,   Cct.primTelephoneInfo = primTel
                    ,   Cct.secTelephoneInfo = secTel
                    }
    | otherwise = error "Invalid contact information"


fromContactInformation :: 
    Cct.ContactInformation -> ContactInformationDto
fromContactInformation ci = 
    let add = uwrpOptPstAddress $ Cct.address ci
        (email, primTel, secTel) = fromContactMethod $ Cct.contactMethod ci 
    in ContactInformationDto {
            email = email
        ,   address = add
        ,   primaryTel = primTel
        ,   secondaryTel = secTel 
        }
  
fromContactMethod :: Cct.ContactMethod -> (String, String, String) 
fromContactMethod (Cct.EmailOnly email) = (uwrpEmailAddress email, "", "")  
fromContactMethod (Cct.PhoneOnly primTel maybeSecTel) = 
    case maybeSecTel  of 
        Nothing -> ("", uwrpTel primTel, "")
        Just wrappedSecTel -> ("", uwrpTel primTel, uwrpTel wrappedSecTel)
fromContactMethod (Cct.EmailAndPhone  both) = 
    let email = uwrpEmailAddress $ Cct.emailInfo both
        primTel = uwrpTel $ Cct.primTelephoneInfo both
        maybeSecTel = Cct.secTelephoneInfo both
    in case maybeSecTel of 
            Just wrappedSecTel -> (email, primTel, uwrpTel wrappedSecTel)
            Nothing -> (email, primTel, "")




-- ----------------------------------------------------------------------------
-- DTO for FullName
-- ----------------------------------------------------------------------------


data FullNameDto = FullNameDto {
      first     :: String
    , middle    :: String
    , last      :: String
    } deriving (Generic, Show)

instance ToJSON FullNameDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FullNameDto

-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedFullName :: FullNameDto -> UnvalidatedFullName
toUnvalidatedFullName =
    UnvalidatedFullName
        <$> first
        <*> middle
        <*> last  
    

toFullName :: FullNameDto -> Either ErrorMessage Cct.FullName
toFullName dto = 
    Cct.FullName  
        <$> (crtFstNm . first) dto
        <*> (crtMdleNm . middle) dto
        <*> (crtLstNm . last) dto
        
fromFullName :: Cct.FullName -> FullNameDto
fromFullName = 
    FullNameDto
        <$> uwrpFstNm . Cct.first 
        <*> uwrpMdleNm . Cct.middle 
        <*> uwrpLstNm . Cct.last 
    




-- ----------------------------------------------------------------------------
-- DTO for DeclareLostItemForm
-- ----------------------------------------------------------------------------




data DeclareLostItemForm = DeclareLostItemForm {
        name :: String
    ,   categoryId :: String
    ,   description :: String
    ,   locations :: [LocationDto]
    ,   dateAndTimeSpan :: (String, String)
    ,   attributes :: [AttributeDto]
    ,   owner :: PersonDto   
    } deriving (Generic, Show)

instance ToJSON DeclareLostItemForm where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DeclareLostItemForm

-- Helper functions for converting from / to domain as well as to other states


---- TODO:  Why is the applicative form broken here?????
toUnvalidatedLostItem :: DeclareLostItemForm -> UnvalidatedLostItem
toUnvalidatedLostItem = 
    UnvalidatedLostItem
        <$> name
        <*> categoryId
        <*> description
        <*> dateAndTimeSpan 
        <*> fmap toUnvalidatedLocation . locations 
        <*> fmap toUnvalidatedAttribute . attributes 
        <*> toUnvalidatedPerson . owner

----------------------------------------------------------------------------
-- DTO for LostItemDeclared  and SearchableItemDeclared Events
-- ----------------------------------------------------------------------------
data LostItemDeclaredDto = LostItemDeclaredDto {
        itemId :: String
    ,   itemName :: String
    ,   itemCategoryId :: String
    ,   itemDescription :: String
    ,   itemLocations :: [LocationDto]
    ,   itemTimeRegistered :: UTCTime
    ,   itemDateTimeSpan :: (String, String)
    ,   itemAttributes :: [AttributeDto]
    ,   itemOwner :: PersonDto 
    } deriving (Generic, Show)


instance ToJSON LostItemDeclaredDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LostItemDeclaredDto

-- Helper functions for converting from / to domain as well as to other states

fromLostItemDeclared :: LostItemDeclared -> LostItemDeclaredDto
fromLostItemDeclared = 
    LostItemDeclaredDto
        <$> uwrpLstItmId . lostItemId 
        <*> uwrpItmNm . lostItemName 
        <*> uwrpCatgrId . lostItemCategoryId 
        <*> uwrpLgDescpt . lostItemDesc 
        <*> fmap fromLocation . toList . lostItemLocation 
        <*> lostItemRegistrationTime 
        <*> uwrpDtTmSpan . lostItemDateAndTimeSpan 
        <*> fmap fromAttribute . toList . lostItemAttributes 
        <*> fromPerson . lostItemOwner  

fromDateTimeSpan :: DateTimeSpan -> (String, String)
fromDateTimeSpan = uwrpDtTmSpan



-- ----------------------------------------------------------------------------
-- DTO for AcknowledgmentSent Event
-- ----------------------------------------------------------------------------

data DeclarationAcknowledgmentSentDto = 
    DeclarationAcknowledgmentSentDto {
        id :: String
    ,   declarantContact :: ContactInformationDto
    } deriving (Generic, Show)


instance ToJSON DeclarationAcknowledgmentSentDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DeclarationAcknowledgmentSentDto

-- Helper functions for converting from / to domain as well as to other states

fromDeclarationAcknowledgmentSent :: 
    DeclarationAcknowledgmentSent -> DeclarationAcknowledgmentSentDto
fromDeclarationAcknowledgmentSent  = 
    DeclarationAcknowledgmentSentDto
        <$> uwrpLstItmId . declaredLostItemId 
        <*> fromContactInformation . ownerContactInfo
    






-- ----------------------------------------------------------------------------
-- DTO for DeclareLostItemError 
-- ----------------------------------------------------------------------------



data DeclareLostItemErrorDto = DeclareLostItemErrorDto {
        code :: String
    ,   message :: String
    } deriving (Generic, Show)

instance ToJSON DeclareLostItemErrorDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DeclareLostItemErrorDto

-- Helper functions for converting from / to domain as well as to other states

fromDeclareLostItemError :: DeclareLostItemError -> DeclareLostItemErrorDto
fromDeclareLostItemError domainError = 
    case domainError of
        Validation (ValidationError errorMessage) ->
            DeclareLostItemErrorDto {
                code = "ValidationError"
            ,   message = errorMessage
            }
        Remote remoteServiceError ->
            let serv = service remoteServiceError 
                -- errorMessage = message $ execption remoteServiceError
            in DeclareLostItemErrorDto {
                code = "RemoteServiceError"
            ,   message = serviceName serv
            }
        Db (DbError errorMessage) ->
            DeclareLostItemErrorDto {
                    code = "DbError"
                ,   message = errorMessage
                } 

--- 

data DeclareLostItemEventDto = 
    LI LostItemDeclaredDto | DA DeclarationAcknowledgmentSentDto deriving (Generic, Show)

instance ToJSON DeclareLostItemEventDto

---

type DeclareLostItemEventResponse = Map String DeclareLostItemEventDto

instance ToJSONKey DeclareLostItemEventResponse

---

data Resp = 
        Success [DeclareLostItemEventResponse] | Error DeclareLostItemErrorDto deriving (Generic, Show)

instance ToJSON Resp


fromDomain :: DeclareLostItemEvent -> DeclareLostItemEventResponse
fromDomain evt = 
    case evt of
        LostItemDeclared lid ->
            let key = "declaredLostItem"
                val = fromLostItemDeclared lid
            in  singleton key (LI val)
        SearchableItemDeclared sid ->
            let key = "searchableLostItem"
                val = fromLostItemDeclared sid
            in  singleton key (LI val)
        AcknowledgmentSent as -> 
            let key = "acknowledgmentSent"
                val = fromDeclarationAcknowledgmentSent as
            in  singleton key (DA val)
        

        



























-- =============================================================================
--  Playground area 
-- =============================================================================
    

{--

data DeclareLostItemEvent =
      LostItemDeclared LostItemDeclared
    | SearchableItemDeclared LostItemDeclared 
    | AcknowledgmentSent DeclarationAcknowledgmentSent
    deriving (Eq, Ord, Show)


--}