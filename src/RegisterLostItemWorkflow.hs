module RegisterLostItemWorkflow where

import DomainCommonTypes
import DomainTypes


import Data.Time
import Prelude hiding (last)
import Data.Maybe


--- Input / Output types
---
---

data UnvalidatedLostItem = UnvalidatedLostItem {
    -- Self data
        uliIdentifier :: String
    ,   uliName :: String
    ,   uliCategoryId :: String
    ,   uliDescription :: String
    -- Location data
    ,   uregion :: String
    ,   udivision :: String
    ,   usubdivision :: String
    ,   ucity :: String
    ,   uvillage :: String
    ,   uneighborhood :: String
    ,   uloaddress :: String
    -- Attributes data
    ,   uliattributes :: [UnvalidatedAttribute]
    -- Owner data
    ,   uuserId :: String
    -- Contact Info
    ,   uemail :: String
    ,   uaddress :: String
    ,   uprimaryTel :: String
    ,   usecondaryTel :: String 
    -- Name    
    ,   ufirst :: String
    ,   umiddle :: String
    ,   ulast :: String    
    } deriving (Eq, Ord, Show)

  
data ValidatedLostItem = ValidatedLostItem {
      -- Self data
        vliIdentifier :: LostItemId
    ,   vliName :: ItemName
    ,   vliCategoryId :: CategoryId
    ,   vliDescription :: LongDescription
    ,   vLostTime :: UTCTime
      -- Lvcation data
    ,   vregion :: Region
    ,   vdivision :: Division
    ,   vsubdivision :: SubDivision
    ,   vcity :: City
    ,   vvillage :: Village
    ,   vneighborhood :: Neighborhood
    ,   vloAddress :: Address
      -- Attributes data
    ,   vliAttributes :: [Attribute]
      -- Owner data
    ,   vuserId :: UserId
      -- Contact Info
    ,   vemail :: EmailAddress
    ,   vaddress :: PostalAddress
    ,   vprimaryTel :: Telephone
    ,   vsecondaryTel :: Telephone 
      -- Name    
    ,   vfirst :: FirstName
    ,   vmiddle :: Maybe Middle
    ,   vlast :: LastName    
    } deriving (Eq, Ord, Show)


data UnvalidatedAttribute = UnvalidatedAttribute {
        uattrCode :: String
      , uattrName :: String
      , uattrDescription :: String
      , uattrValue :: String
      , uattrUnit ::   String
      , urelatedCategory :: String
      , urelatedCategoryType :: String
    } deriving (Eq, Ord, Show)

-- Output types
data LostItemDto = LostItemDto {
        _id :: String
    ,   _type :: String
    ,   _name :: String
    ,   _category_id :: String
    ,   _lost_location :: LocationDto
    ,   _description :: String
    ,   _attributes :: [AttributeDto]
    ,   _owner :: PersonDto
    } deriving (Eq, Ord, Show)


data LocationDto = LocationDto {
        _region_code :: String
    ,   _division_code :: String
    ,   _subdivision_code :: String
    ,   _city :: String
    ,   _village :: String
    ,   _neighborhood :: String
    ,   _location_address :: String
    } deriving (Eq, Ord, Show)


data AttributeDto = AttributeDto {
        _attr_code :: String
      , _attr_name :: String
      , _attr_description :: String
      , _attribute_value :: String
      , _attribute_unit ::   String
      , _related_category_id :: String
      , _related_category_type :: String
    } deriving (Eq, Ord, Show)


data PersonDto = PersonDto {
      _user_id :: String
    , _contact_info :: ContactInformationDto
    , _full_name :: FullNameDto
    } deriving (Eq, Ord, Show)


data ContactInformationDto = ContactInformationDto {
    -- Tel required, email optional
      _email :: String
    , _postal_address :: String
    , _primary_telephone :: String
    , _secondary_telephone :: String
    } deriving (Eq, Ord, Show)
  

data FullNameDto = FullNameDto {
      _first :: String
    , _middle :: String
    , _last :: String
    } deriving (Eq, Ord, Show)



--- Step 1: Validate UnvalidatedLostItem
---
---

--- Step type
type ValidateUnvalidatedLostItem = 
  UnvalidatedLostItem -> UTCTime -> Either ErrorMessage ValidatedLostItem

--- Step implementation
validateUnvalidatedLostItem :: ValidateUnvalidatedLostItem
validateUnvalidatedLostItem u t = 
  ValidatedLostItem 
    <$> (createLostItemId . uliIdentifier ) u
    <*> (createItemName . uliName) u
    <*> (createCategoryId . uliCategoryId) u
    <*> (createLongDescription . uliDescription) u
    <*> Right t
    <*> (toRegion . uregion) u
    <*> (toDivision . udivision) u
    <*> (toSubDivision . usubdivision) u
    <*> (createCity . ucity) u
    <*> (createVillage . uvillage) u
    <*> (createNeighborhood . uneighborhood) u
    <*> (createAddress . uloaddress) u
    <*> (createAttributes . uliattributes) u
    <*> (createUserId . uuserId) u
    <*> (createEmailAddress . uemail) u
    <*> (createPostalAddress . uaddress) u
    <*> (createTelephone . uprimaryTel) u
    <*> (createTelephone . usecondaryTel) u
    <*> (createFirstName . ufirst) u
    <*> (createMiddle . umiddle) u
    <*> (createLastName . ulast) u

--- Helper function      
createAttributes :: [UnvalidatedAttribute] -> Either ErrorMessage [Attribute]
createAttributes attrs = 
  let res = fmap toAttribute attrs
  in sequence res

toAttribute :: UnvalidatedAttribute -> Either ErrorMessage Attribute
toAttribute u = 
  do code <- createAttributeCode $ uattrCode  u
     name <- createAttributeName $ uattrName u
     description <- createShortDescription $ uattrDescription u
     value <- createAttributeValue $ uattrValue u
     unit <- createAttributeUnit $ uattrUnit u
     categoryId <- createCategoryId $ urelatedCategory u
     categoryType <- toCategoryType $ urelatedCategoryType u
     return $ Attribute code name description (Just value) (Just unit) 
                        categoryId categoryType

                        


--- Step 2: Create LostItem
---
---

type CreateLostItem = 
  ValidatedLostItem -> Item

--- Step implementatin
createLostItem :: CreateLostItem
createLostItem v = 

  let location =
        Location
          (vregion v)         
          (vdivision v)
          (vsubdivision v)
          (vcity v)
          (vvillage v)
          (vneighborhood v)
          (vloAddress v)
  
      fullName = 
        FullName 
          (vfirst v)
          (vmiddle v)
          (vlast v)
  
      contactInfo = 
        ContactInformation
          (vemail v)
          (vaddress v)
          (vprimaryTel v)
          (vsecondaryTel v)
      owner = 
        Person
          (vuserId v)
          contactInfo
          fullName

      lostItem = 
        LostItem
          (vliIdentifier v)         
          (vliName v)
          (vliCategoryId v)
          location
          (vliDescription v)
          (vLostTime v)
          (vliAttributes v)
          owner
  in Lost lostItem
      



--- Step 3: Construct Dtos

type CreateDto = 
  Item -> LostItemDto

--- Step implementation
createDto :: CreateDto
createDto (Lost item) =
  let 
    location = LocationDto {
          _region_code = fromRegion $ (region . lostItemLocation) item
      ,   _division_code = fromDivision $ (division . lostItemLocation) $ item
      ,   _subdivision_code = fromSubDivision $ (subdivision . lostItemLocation) $ item
      ,   _city = unwrapCity $ (city . lostItemLocation) item
      ,   _village = unwrapVillage $ (village . lostItemLocation) item
      ,   _neighborhood = unwrapNeighborhood $ (neighborhood . lostItemLocation) item
      ,   _location_address = unwrapAddress $ (locationAddress . lostItemLocation) item
      }

    contactInfo = ContactInformationDto {
        _email = unwrapEmailAddress $ (email . contact . lostItemOwner) item
      , _postal_address = unwrapPostalAddress $ (address . contact . lostItemOwner) item
      , _primary_telephone = unwrapTelephone $ ( primaryTel . contact . lostItemOwner) item
      , _secondary_telephone = unwrapTelephone $ (secondaryTel . contact . lostItemOwner) item
      }

    fullName = FullNameDto {
        _first = unwrapFirstName $ (first . name . lostItemOwner ) item
      , _middle = unwrapMiddle $ (middle . name . lostItemOwner ) item
      , _last = unwrapLastName $ (last . name . lostItemOwner ) item
      }

    owner = PersonDto {
       _user_id = unwrapUserId $ (userId . lostItemOwner) item
      , _contact_info = contactInfo
      , _full_name = fullName
      }

  in LostItemDto {
          _id = unwrapLostItemId $ lostItemId item
      ,   _type = itemTypeToString (Lost item)
      ,   _name = unwrapItemName $ lostItemName item
      ,   _category_id = unwrapCategoryId $ lostItemCategoryId item
      ,   _lost_location = location
      ,   _description = unwrapLongDescription $ lostItemDesc item
      ,   _attributes = fmap fromAttribute $ lostItemAttributes item
      ,   _owner = owner
      }
 

--- Helper functions



fromAttribute :: Attribute -> AttributeDto
fromAttribute attr = 
  let attrDto = AttributeDto {
         _attr_code = unwrapAttributeCode $ attrCode attr
        , _attr_name = unwrapAttributeName $ attrName attr
        , _attr_description = unwrapShortDescription $ attrDescription attr
        , _attribute_value = unwrapAttributeValue $ attrValue attr
        , _attribute_unit = unwrapAttributeUnit $ attrUnit attr
        , _related_category_id = unwrapCategoryId $ relatedCategory attr
        , _related_category_type = fromCategoryType $ relatedCategoryType attr
        }
  in attrDto 



--- Workflow Type

type RegisterLostItemWorkflow = 
      UnvalidatedLostItem -> UTCTime -> Either ErrorMessage LostItemDto


registerLostItemWorkflow :: RegisterLostItemWorkflow
registerLostItemWorkflow unvalidatedLostItem time = do
  validatedLostItem <- validateUnvalidatedLostItem unvalidatedLostItem time
  lostItem <- return $ createLostItem validatedLostItem
  lostItemDto <- return $ createDto lostItem
  return lostItemDto



  






-- Pandoc need to look into it. 
-- Which IDE 

-- Merge and quicksort better for functional languages
-- NVM type drive






