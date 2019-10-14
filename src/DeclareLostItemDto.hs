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
    ,   dtolocationAddress :: String
    } deriving (Eq, Ord, Show)

-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedLocation :: LocationDto -> UnvalidatedLocation
toUnvalidatedLocation  =
    UnvalidatedLocation 
        <$> dtoregion 
        <*> dtodivision 
        <*> dtosubdivision
        <*> dtocity
        <*> dtovillage 
        <*> dtoneighborhood 
        <*> dtolocationAddress
        


---- TODO:  I think applicative might be welcome here and in many other places :) 
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
fromLocation = 
    LocationDto  
        <$> (fromRegion . region) 
        <*> (fromDivision . division) 
        <*> (fromSubDivision . subdivision)
        <*> (unwrapCity . city)
        <*> (unwrapVillage . village) 
        <*> (unwrapNeighborhood . neighborhood) 
        <*> (unwrapAddress . locationAddress) 




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
toUnvalidatedAttribute = 
    UnvalidatedAttribute 
        <$> dtoattrCode 
        <*> dtoattrName 
        <*> dtoattrDescription 
        <*> dtoattrValue 
        <*> dtoattrUnit 
        <*> dtorelatedCategories 
    

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
fromAttribute  = 
    AttributeDto 
        <$> unwrapAttributeCode . attrCode 
        <*> unwrapAttributeName . attrName 
        <*> unwrapShortDescription . attrDescription 
        <*> unwrapAttributeValue . attrValue 
        <*> unwrapAttributeUnit . attrUnit 
        <*> fmap fromCategoryIdAndCategoryType . relatedCategories 
    
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
fromContactInformation = 
    ContactInformationDto
        <$> unwrapEmailAddress . email 
        <*> unwrapPostalAddress . address 
        <*> unwrapTelephone . primaryTel 
        <*> unwrapTelephone . secondaryTel  
    



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
    }


-- Helper functions for converting from / to domain as well as to other states


---- TODO:  Why is the applicative form broken here?????
toUnvalidatedLostItem :: DeclareLostItemForm -> UnvalidatedLostItem
toUnvalidatedLostItem dtoForm = 
    UnvalidatedLostItem {
        uliName =               fname dtoForm
    ,   uliCategoryId =         fcategoryId dtoForm
    ,   uliDescription =        fdescription dtoForm
    ,   ulocations =            fmap toUnvalidatedLocation $ flocations dtoForm
    ,   uliDateAndTimeSpan =    fDateAndTimeSpan dtoForm
    ,   uliattributes           fmap toUnvalidatedAttribute $ fattributes dtoForm
    ,   uowner =                toUnvalidatedPerson $ fowner dtoForm   
    }

{-
    UnvalidatedLostItem {
        uliName =               fname dtoForm
    ,   uliCategoryId =         fcategoryId dtoForm
    ,   uliDescription =        fdescription dtoForm
    ,   ulocations =            fmap toUnvalidatedLocation $ flocations dtoForm
    ,   uliDateAndTimeSpan =    fDateAndTimeSpan dtoForm
    ,   uliattributes =         fmap toUnvalidatedAttribute $ fattributes dtoForm
    ,   uowner =                toUnvalidatedPerson $ fowner dtoForm   
    }
-}

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