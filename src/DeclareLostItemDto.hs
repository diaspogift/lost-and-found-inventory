module DeclareLostItemDto where

import CommonSimpleTypes
import CommonCompoundTypes
import DeclaredLostItemPublicTypes


import Prelude hiding (last, id)
import Data.Time
import Data.Set



-- ==========================================================================================
-- This file contains the the logic for working with data transfer objects (DTOs)
--
-- Each type of DTO is defined using primitives, serializable types and there are 
-- toDomain and fromDomain functions defined for each DTO.
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
    ,   dtolocationAddress :: String
    } deriving (Eq, Ord, Show)

-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedLocation :: LocationDto -> UnvalidatedLocation
toUnvalidatedLocation dto =
    UnvalidatedLocation {
        uregion = dtoregion dto
    ,   udivision = dtodivision dto
    ,   usubdivision = dtosubdivision dto
    ,   ucity = dtocity dto
    ,   uvillage = dtovillage dto
    ,   uneighborhood = dtoneighborhood dto
    ,   uloaddress = dtolocationAddress dto
    }

toLocation :: LocationDto -> Either ErrorMessage Location
toLocation dto = 
    do  reg <- toRegion $ dtoregion dto
        div <- toDivision $ dtodivision dto
        sub <- toSubDivision $ dtosubdivision dto
        cit <- createCity $ dtocity dto
        vil <- createVillage $ dtovillage dto
        nei <- createNeighborhood $ dtoneighborhood dto
        add <- createAddress $ dtolocationAddress dto
        return  Location {
                    region = reg 
                ,   division = div
                ,   subdivision = sub
                ,   city = cit
                ,   village = vil
                ,   neighborhood = nei
                ,   locationAddress = add
                }

fromLocation :: Location -> LocationDto
fromLocation domain = 
    LocationDto {
        dtoregion = fromRegion $ region domain
    ,   dtodivision = fromDivision $ division domain
    ,   dtosubdivision = fromSubDivision $ subdivision domain
    ,   dtocity = unwrapCity $ city domain
    ,   dtovillage = unwrapVillage $ village domain
    ,   dtoneighborhood = unwrapNeighborhood $ neighborhood domain
    ,   dtolocationAddress = unwrapAddress $ locationAddress domain
    }



-- ----------------------------------------------------------------------------
-- DTO for Attribute
-- ----------------------------------------------------------------------------



data AttributeDto = AttributeDto {
      dtoattrCode             :: String
    , dtoattrName             :: String
    , dtoattrDescription      :: String
    , dtoattrValue            :: String
    , dtoattrUnit             :: String
    , dtorelatedCategories    :: [(String, String)] 
    } deriving (Eq, Ord, Show)


-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedAttribute :: AttributeDto -> UnvalidatedAttribute 
toUnvalidatedAttribute dto = 
    UnvalidatedAttribute {
      uattrCode = dtoattrCode dto
    , uattrName = dtoattrName dto
    , uattrDescription = dtoattrDescription dto
    , uattrValue = dtoattrValue dto
    , uattrUnit = dtoattrUnit dto
    , urelatedCategories = dtorelatedCategories dto
    }

toAttribute :: AttributeDto -> Either ErrorMessage Attribute
toAttribute dto = 
    do  code <- createAttributeCode $ dtoattrCode dto
        name <- createAttributeName $ dtoattrName dto
        desc <- createShortDescription $ dtoattrDescription dto
        val <- createAttributeValue $ dtoattrValue dto
        unit <- createAttributeUnit $ dtoattrUnit dto
        catIds <- sequence $ fmap toCatIdCatTypePair $ dtorelatedCategories dto

        return  Attribute {
                  attrCode = code
                , attrName = name
                , attrDescription = desc
                , attrValue = Just val
                , attrUnit = Just unit
                , relatedCategories = catIds
                }
        where 
            toCatIdCatTypePair (strCatId, strCatType) =
                do  catId <- createCategoryId strCatId
                    catType <- toCategoryType strCatType
                    return (catId, catType)

fromAttribute :: Attribute -> AttributeDto
fromAttribute domain = 
    AttributeDto {
      dtoattrCode = unwrapAttributeCode $ attrCode domain
    , dtoattrName = unwrapAttributeName $ attrName domain
    , dtoattrDescription = unwrapShortDescription $ attrDescription domain
    , dtoattrValue = unwrapAttributeValue $ attrValue domain
    , dtoattrUnit = unwrapAttributeUnit $ attrUnit domain
    , dtorelatedCategories = fmap fromCategoryIdAndCategoryType $ relatedCategories domain
    }
    where fromCategoryIdAndCategoryType (catId, catType) =
            (unwrapCategoryId catId, fromCategoryType catType)




-- ----------------------------------------------------------------------------
-- DTO for Person
-- ----------------------------------------------------------------------------

data PersonDto = PersonDto {
        dtouserId :: String
    ,   dtocontact :: ContactInformationDto
    ,   dtofullname :: FullNameDto
    }


-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedPerson :: PersonDto -> UnvalidatedPerson
toUnvalidatedPerson dto = 
    UnvalidatedPerson {
        uuserId = dtouserId dto
    ,   ucontact = toUnvalidatedContactInformation $ dtocontact dto
    ,   ufullname = toUnvalidatedFullName $ dtofullname dto
    }

fromPerson :: Person -> PersonDto
fromPerson domain = PersonDto {
        dtouserId = unwrapUserId $ userId domain
    ,   dtocontact = fromContactInformation $ contact domain
    ,   dtofullname = fromFullName $ name domain
    }


-- ----------------------------------------------------------------------------
-- DTO for ContactInformation
-- ----------------------------------------------------------------------------


data ContactInformationDto = ContactInformationDto {
        dtoemail :: String
    ,   dtoaddress :: String
    ,   dtoprimaryTel :: String
    ,   dtosecondaryTel :: String 
    } deriving (Eq, Ord, Show)


-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedContactInformation :: ContactInformationDto -> UnvalidatedContactInformation
toUnvalidatedContactInformation dto =
    UnvalidatedContactInformation {
        uemail = dtoemail dto
    ,   uaddress = dtoaddress dto
    ,   uprimaryTel = dtoprimaryTel dto
    ,   usecondaryTel = dtosecondaryTel dto
    }

toContactInformation :: ContactInformationDto -> Either ErrorMessage ContactInformation
toContactInformation dto = 
    do  ema <- createEmailAddress $ dtoemail dto
        add <- createPostalAddress $ dtoaddress dto
        pri <- createTelephone $ dtoprimaryTel dto
        sec <- createTelephone $ dtosecondaryTel dto
        return  ContactInformation {
                -- Tel required, email optional
                  email = ema
                , address = add
                , primaryTel = pri
                , secondaryTel = sec
                }

fromContactInformation :: ContactInformation -> ContactInformationDto
fromContactInformation domain = 
    ContactInformationDto {
        dtoemail = unwrapEmailAddress $ email domain
    ,   dtoaddress = unwrapPostalAddress $ address domain
    ,   dtoprimaryTel = unwrapTelephone $ primaryTel domain
    ,   dtosecondaryTel = unwrapTelephone $ secondaryTel domain 
    }



-- ----------------------------------------------------------------------------
-- DTO for FullName
-- ----------------------------------------------------------------------------


data FullNameDto = FullNameDto {
      dtofirst     :: String
    , dtomiddle    :: String
    , dtolast      :: String
    } deriving (Eq, Ord, Show)


-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedFullName :: FullNameDto -> UnvalidatedFullName
toUnvalidatedFullName dto =
    UnvalidatedFullName {
        ufirst = dtofirst dto
    ,   umiddle = dtomiddle dto
    ,   ulast = dtolast dto  
    }

toFullName :: FullNameDto -> Either ErrorMessage FullName
toFullName dto = 
    do  fir <- createFirstName $ dtofirst dto
        mid <- createMiddle $ dtomiddle dto
        las <- createLastName $ dtolast dto
        return FullName {
                  first = fir
                , middle = mid
                , last = las
                }

fromFullName :: FullName -> FullNameDto
fromFullName domain = 
    FullNameDto {
      dtofirst = unwrapFirstName $ first domain
    , dtomiddle = unwrapMiddle $ middle domain
    , dtolast = unwrapLastName $ last domain
    }




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
    }


-- Helper functions for converting from / to domain as well as to other states



toUnvalidatedLostItem :: DeclareLostItemForm -> UnvalidatedLostItem
toUnvalidatedLostItem dtoForm = 
    UnvalidatedLostItem {
        uliName = fname dtoForm
    ,   uliCategoryId = fcategoryId dtoForm
    ,   uliDescription = fdescription dtoForm
    ,   ulocations = fmap toUnvalidatedLocation $ flocations dtoForm
    ,   uliDateAndTimeSpan = fDateAndTimeSpan dtoForm
    ,   uliattributes = fmap toUnvalidatedAttribute $ fattributes dtoForm
    ,   uowner = toUnvalidatedPerson $ fowner dtoForm   
    }



-- ----------------------------------------------------------------------------
-- DTO for LostItemDeclared  and SearchableItemDeclared Events
-- ----------------------------------------------------------------------------
data LostItemDeclaredDto = LostItemDeclaredDto {
        dtoitemId :: String
    ,   dtoname :: String
    ,   dtocategoryId :: String
    ,   dtoescription :: String
    ,   dtolocations :: [LocationDto]
    ,   dtotimeRegistered :: UTCTime
    ,   dtodatetimeSpan :: DateTimeSpan
    ,   dtoattributes :: [AttributeDto]
    ,   dtoowner :: PersonDto 
    }


-- Helper functions for converting from / to domain as well as to other states

fromLostItemDeclared :: LostItemDeclared -> LostItemDeclaredDto
fromLostItemDeclared domain = 
    LostItemDeclaredDto {
        dtoitemId = unwrapLostItemId $ lostItemId domain
    ,   dtoname = unwrapItemName $ lostItemName domain
    ,   dtocategoryId = unwrapCategoryId $ lostItemCategoryId domain
    ,   dtoescription = unwrapLongDescription $ lostItemDesc domain
    ,   dtolocations = (fmap fromLocation) $ toList $ lostItemLocation domain
    ,   dtotimeRegistered = lostItemRegistrationTime domain
    ,   dtodatetimeSpan = lostItemDateAndTimeSpan domain
    ,   dtoattributes =  fmap fromAttribute $ toList $ lostItemAttributes domain
    ,   dtoowner = fromPerson $ lostItemOwner domain 
    }


-- ----------------------------------------------------------------------------
-- DTO for AcknowledgmentSent Event
-- ----------------------------------------------------------------------------

data DeclarationAcknowledgmentSentDto = DeclarationAcknowledgmentSentDto {
        id :: String
    ,   declarantContact :: ContactInformationDto
    }


-- Helper functions for converting from / to domain as well as to other states

fromDeclarationAcknowledgmentSent :: DeclarationAcknowledgmentSent -> DeclarationAcknowledgmentSentDto
fromDeclarationAcknowledgmentSent domain = 
    DeclarationAcknowledgmentSentDto {
        id = unwrapLostItemId $ declaredLostItemId domain
    ,   declarantContact = fromContactInformation $ ownerContactInfo domain
    }






-- ----------------------------------------------------------------------------
-- DTO for DeclareLostItemError 
-- ----------------------------------------------------------------------------



data DeclareLostItemErrorDto = 
    DeclareLostItemErrorDto {
        code :: String
    ,   message :: String
    }


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