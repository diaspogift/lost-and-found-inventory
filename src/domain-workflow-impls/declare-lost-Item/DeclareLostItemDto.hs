{-# LANGUAGE DeriveGeneric #-}


module DeclareLostItemDto where


import Data.Aeson
import CommonSimpleTypes
import CommonCompoundTypes
import DeclaredLostItemPublicTypes


import Prelude hiding (last, id)
import Data.Time
import Data.Set hiding (null)

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
        dtoregion :: String
    ,   dtodivision :: String
    ,   dtosubdivision :: String
    ,   dtocity :: String
    ,   dtovillage :: String
    ,   dtoneighborhood :: String
    ,   dtolocationAddress :: [String]
    } deriving (Generic, Show)

instance ToJSON LocationDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LocationDto


-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedLocation :: LocationDto -> UnvalidatedLocation
toUnvalidatedLocation dto  =
    let area = (dtoregion dto, dtodivision dto, dtosubdivision dto)
        city = dtocity dto
        village = dtovillage dto
        neighborhood = dtoneighborhood dto
        addresses = dtolocationAddress dto
    in UnvalidatedLocation {
            uadminArea = area
        ,   ucity = city
        ,   uvillage = village
        ,   uneighborhood = neighborhood
        ,   uloaddresses = addresses
        }
        


toLocation :: LocationDto -> Either ErrorMessage Location
toLocation dto = 
    do  area <- toAdminAreaInfo (dtoregion dto, dtodivision dto, dtosubdivision dto)
        lieu <- toCityOrVillage (dtocity dto, dtovillage dto)
        voisinage <- createOptionalNeighborhood $ dtoneighborhood dto
        addresses <- traverse toAddress $ dtolocationAddress dto
        return Location  {
                    adminArea = area
                ,   cityOrVillage = lieu
                ,   neighborhood = voisinage
                ,   locationAddresses = addresses
                }


toAdminAreaInfo :: 
    (String, String, String) -> Either ErrorMessage (Maybe AdministrativeAreaInfo) 
toAdminAreaInfo (strReg, strDiv, strSub)
    | null strReg && null strDiv, null strSub = Right Nothing
    | otherwise = 
        do  reg <- toRegion strReg
            div <- toDivision strDiv
            sub <- toSubDivision strSub
            return $ Just (reg, div, sub)

toCityOrVillage :: 
    (String, String) 
    -> Either ErrorMessage (Maybe CityOrVillage)
toCityOrVillage (strCity, strVillage)
    | isStringNull strCity && isStringNull strVillage = 
        return Nothing
    | isStringNull strCity && isStringNotNull strVillage = 
        do  village <- createVillage strVillage
            return $ Just $ Country village
    | isStringNotNull strCity && isStringNull strVillage = 
        do  city <- createCity strCity
            return $ Just $ Urban city
    | otherwise = return Nothing 
    where   isStringNotNull = (not . null)
            isStringNull = null



toAddress :: String -> Either ErrorMessage Address
toAddress strAddress = createAddress strAddress

fromLocation :: Location -> LocationDto
fromLocation loc = 
    let (reg, div, sub) = fromMaybeAdminArea $ adminArea loc
        (city, village) = fromMaybeCityOrVillage $ cityOrVillage loc
        maybeNeighborhood = fromMaybeNeighborhood $ neighborhood loc
        addresses = fmap unwrapAddress $ locationAddresses loc
    in LocationDto {
            dtoregion = reg
        ,   dtodivision = div
        ,   dtosubdivision = sub
        ,   dtocity = city
        ,   dtovillage = village
        ,   dtoneighborhood = maybeNeighborhood
        ,   dtolocationAddress = addresses
        }

fromMaybeAdminArea :: Maybe AdministrativeAreaInfo -> (String, String, String)
fromMaybeAdminArea Nothing = ("", "", "")
fromMaybeAdminArea (Just (reg, div, sub)) = (fromRegion reg, fromDivision div, fromSubDivision sub)

fromMaybeCityOrVillage :: Maybe CityOrVillage -> (String, String)
fromMaybeCityOrVillage (Just (Urban wCity))= (unwrapCity wCity, "")
fromMaybeCityOrVillage (Just (Country wVillage))= ("", unwrapVillage wVillage)
fromMaybeCityOrVillage Nothing = ("","")


fromMaybeNeighborhood :: Maybe Neighborhood -> String
fromMaybeNeighborhood (Just wNeighborhood) = unwrapNeighborhood wNeighborhood
fromMaybeNeighborhood Nothing = ""

-- ----------------------------------------------------------------------------
-- DTO for Attribute
-- ----------------------------------------------------------------------------



data AttributeDto = AttributeDto {
      dtoattrCode             :: String
    , dtoattrName             :: String
    , dtoattrDescription      :: String
    , dtoattrValue            :: String
    , dtoattrUnit             :: String
    } deriving (Generic, Show)

instance ToJSON AttributeDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AttributeDto

-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedAttribute :: AttributeDto -> UnvalidatedAttribute 
toUnvalidatedAttribute = 
    UnvalidatedAttribute 
        <$> dtoattrCode 
        <*> dtoattrName 
        <*> dtoattrDescription 
        <*> dtoattrValue 
        <*> dtoattrUnit 
    

toAttribute :: AttributeDto -> Either ErrorMessage Attribute
toAttribute dto = 
    do  code <- createAttributeCode $ dtoattrCode dto
        name <- createAttributeName $ dtoattrName dto
        desc <- createShortDescription $ dtoattrDescription dto
        val <- createOptionalAttributeValue $ dtoattrValue dto
        unit <- createOptionalAttributeUnit $ dtoattrUnit dto

        return  Attribute {
                  attrCode = code
                , attrName = name
                , attrDescription = desc
                , attrValue = val
                , attrUnit = unit
                }
        where 
            toCatIdCatTypePair (strCatId, strCatType) =
                do  catId <- createCategoryId strCatId
                    catType <- toCategoryType strCatType
                    return (catId, catType)

fromAttribute :: Attribute -> AttributeDto
fromAttribute  = 
    AttributeDto 
        <$> unwrapAttributeCode . attrCode 
        <*> unwrapAttributeName . attrName 
        <*> unwrapShortDescription . attrDescription 
        <*> unwrapAttributeValue . attrValue 
        <*> unwrapAttributeUnit . attrUnit 





-- ----------------------------------------------------------------------------
-- DTO for Person
-- ----------------------------------------------------------------------------

data PersonDto = PersonDto {
        dtouserId :: String
    ,   dtocontact :: ContactInformationDto
    ,   dtofullname :: FullNameDto
    } deriving (Generic, Show)


instance ToJSON PersonDto where
    toEncoding = genericToEncoding defaultOptions
    
instance FromJSON PersonDto

-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedPerson :: PersonDto -> UnvalidatedPerson
toUnvalidatedPerson = 
    UnvalidatedPerson
        <$> dtouserId 
        <*> toUnvalidatedContactInformation . dtocontact
        <*> toUnvalidatedFullName . dtofullname 


fromPerson :: Person -> PersonDto
fromPerson = 
    PersonDto
        <$> unwrapUserId . userId 
        <*> fromContactInformation . contact 
        <*> fromFullName . name 
    


-- ----------------------------------------------------------------------------
-- DTO for ContactInformation
-- ----------------------------------------------------------------------------


data ContactInformationDto = ContactInformationDto {
        dtoemail :: String
    ,   dtoaddress :: String
    ,   dtoprimaryTel :: String
    ,   dtosecondaryTel :: String 
    } deriving (Generic, Show)


instance ToJSON ContactInformationDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ContactInformationDto

-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedContactInformation :: 
    ContactInformationDto -> UnvalidatedContactInformation
toUnvalidatedContactInformation =
    UnvalidatedContactInformation
        <$> dtoemail 
        <*> dtoaddress 
        <*> dtoprimaryTel 
        <*> dtosecondaryTel 
    

toContactInformation ::
     ContactInformationDto -> Either ErrorMessage ContactInformation
toContactInformation dto = 
    do add <- (createPostalAddress . dtoaddress) dto
       contact <- toContactMethod (dtoemail dto, dtoprimaryTel dto, dtosecondaryTel dto)
       return ContactInformation {
                    address = add
                ,   contactMethod = contact
                }
        

toContactMethod :: (String, String, String) -> Either ErrorMessage ContactMethod
toContactMethod (givenEmail, givenPrimTel, givenSecTel)
    -- no email but both prim and sec phone given
    | null givenEmail 
      && (not . null) givenPrimTel 
      && (not . null) givenSecTel =
        do  primTel <- createTelephone givenPrimTel
            secTel <- createOptionalTelephone givenSecTel
            return $ PhoneOnly primTel secTel
          

    -- no email but only prim phone given
    | null givenEmail
      && (not . null) givenPrimTel 
      && null givenSecTel =
        do  primTel <- createTelephone givenPrimTel
            return $ PhoneOnly primTel Nothing
            

    -- just email given
    | (not. null) givenEmail
      && null givenPrimTel 
      && null givenSecTel =
        do  email <- createEmailAddress givenEmail
            return $ EmailOnly email
            
    -- email and prim phone given
    | (not . null) givenEmail
      && (not . null) givenPrimTel
      && null givenSecTel =
        do  primTel <- createTelephone givenPrimTel
            email <- createEmailAddress givenEmail
            return $ 
                EmailAndPhone BothContactInfo {
                        emailInfo = email
                    ,   primTelephoneInfo = primTel
                    ,   secTelephoneInfo = Nothing
                    }
            
    -- email, prim and sec phones given
    | (not . null) givenEmail 
      && (not . null)  givenPrimTel  
      && (not . null) givenSecTel =
        do  primTel <- createTelephone givenPrimTel
            email <- createEmailAddress givenEmail
            secTel <- createOptionalTelephone givenSecTel
            return $ 
                EmailAndPhone BothContactInfo {
                        emailInfo = email
                    ,   primTelephoneInfo = primTel
                    ,   secTelephoneInfo = secTel
                    }
    | otherwise = error "Invalid contact information"


fromContactInformation :: 
    ContactInformation -> ContactInformationDto
fromContactInformation ci = 
    let add = unwrapPostalAddress $ address ci
        (email, primTel, secTel) = fromContactMethod $ contactMethod ci 
    in ContactInformationDto {
            dtoemail = email
        ,   dtoaddress = add
        ,   dtoprimaryTel = primTel
        ,   dtosecondaryTel = secTel 
        }
  
fromContactMethod :: ContactMethod -> (String, String, String) 
fromContactMethod (EmailOnly email) = (unwrapEmailAddress email, "", "")  
fromContactMethod (PhoneOnly primTel maybeSecTel) = 
    case maybeSecTel  of 
        Nothing -> ("", unwrapTelephone primTel, "")
        Just wrappedSecTel -> ("", unwrapTelephone primTel, unwrapTelephone wrappedSecTel)
fromContactMethod (EmailAndPhone  both) = 
    let email = unwrapEmailAddress $ emailInfo both
        primTel = unwrapTelephone $ primTelephoneInfo both
        maybeSecTel = secTelephoneInfo both
    in case maybeSecTel of 
            Just (wrappedSecTel) -> (email, primTel, unwrapTelephone wrappedSecTel)
            Nothing -> (email, primTel, "")




-- ----------------------------------------------------------------------------
-- DTO for FullName
-- ----------------------------------------------------------------------------


data FullNameDto = FullNameDto {
      dtofirst     :: String
    , dtomiddle    :: String
    , dtolast      :: String
    } deriving (Generic, Show)

instance ToJSON FullNameDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FullNameDto

-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedFullName :: FullNameDto -> UnvalidatedFullName
toUnvalidatedFullName =
    UnvalidatedFullName
        <$> dtofirst
        <*> dtomiddle
        <*> dtolast  
    

toFullName :: FullNameDto -> Either ErrorMessage FullName
toFullName dto = 
    FullName  
        <$> (createFirstName . dtofirst) dto
        <*> (createMiddle . dtomiddle) dto
        <*> (createLastName . dtolast) dto
        
fromFullName :: FullName -> FullNameDto
fromFullName = 
    FullNameDto
        <$> unwrapFirstName . first 
        <*> unwrapMiddle . middle 
        <*> unwrapLastName . last 
    




-- ----------------------------------------------------------------------------
-- DTO for DeclareLostItemForm
-- ----------------------------------------------------------------------------




data DeclareLostItemForm = DeclareLostItemForm {
        fname :: String
    ,   fcategoryId :: String
    ,   fdescription :: String
    ,   flocations :: [LocationDto]
    ,   fDateAndTimeSpan :: (String, String)
    ,   fattributes :: [AttributeDto]
    ,   fowner :: PersonDto   
    } deriving (Generic, Show)

instance ToJSON DeclareLostItemForm where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DeclareLostItemForm

-- Helper functions for converting from / to domain as well as to other states


---- TODO:  Why is the applicative form broken here?????
toUnvalidatedLostItem :: DeclareLostItemForm -> UnvalidatedLostItem
toUnvalidatedLostItem = 
    UnvalidatedLostItem
        <$> fname
        <*> fcategoryId
        <*> fdescription
        <*> fDateAndTimeSpan 
        <*> fmap toUnvalidatedLocation . flocations 
        <*> fmap toUnvalidatedAttribute . fattributes 
        <*> toUnvalidatedPerson . fowner

----------------------------------------------------------------------------
-- DTO for LostItemDeclared  and SearchableItemDeclared Events
-- ----------------------------------------------------------------------------
data LostItemDeclaredDto = LostItemDeclaredDto {
        dtoitemId :: String
    ,   dtoname :: String
    ,   dtocategoryId :: String
    ,   dtoescription :: String
    ,   dtolocations :: [LocationDto]
    ,   dtotimeRegistered :: UTCTime
    ,   dtodatetimeSpan :: (String, String)
    ,   dtoattributes :: [AttributeDto]
    ,   dtoowner :: PersonDto 
    } deriving (Generic, Show)


instance ToJSON LostItemDeclaredDto where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LostItemDeclaredDto

-- Helper functions for converting from / to domain as well as to other states

fromLostItemDeclared :: LostItemDeclared -> LostItemDeclaredDto
fromLostItemDeclared = 
    LostItemDeclaredDto
        <$> unwrapLostItemId . lostItemId 
        <*> unwrapItemName . lostItemName 
        <*> unwrapCategoryId . lostItemCategoryId 
        <*> unwrapLongDescription . lostItemDesc 
        <*> (fmap fromLocation) . toList . lostItemLocation 
        <*> lostItemRegistrationTime 
        <*> unwrapDateTimeSpan . lostItemDateAndTimeSpan 
        <*> (fmap fromAttribute) . toList . lostItemAttributes 
        <*> fromPerson . lostItemOwner  

fromDateTimeSpan :: DateTimeSpan -> (String, String)
fromDateTimeSpan dateTimeSpan = 
    unwrapDateTimeSpan dateTimeSpan



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
        <$> unwrapLostItemId . declaredLostItemId 
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































-- =============================================================================
--  Playground area 
-- =============================================================================
    

data PersonTest = PersonTest {
      tname :: String
    , tage  :: Int
    } deriving (Generic, Show)




instance ToJSON PersonTest where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PersonTest