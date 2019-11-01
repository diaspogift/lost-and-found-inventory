
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}



module CreateAttributeDto where


import Data.Aeson
import CommonSimpleTypes
import CommonDtos
import CommonCompoundTypes
import CreateAttributePublicTypes


import Prelude hiding (last, id)
import Data.Time
import Data.Set hiding (null, singleton)
import Data.Map hiding (null, toList)

import GHC.Generics







-- ==========================================================================================
-- This file contains the the logic for working with data transfer objects (DTOs)
--
-- Each type of DTO is defined using primitive, serializable types and there are 
-- toUnvlidated, toDomain and fromDclLstItmEvtDomain functions defined for each of them.
--
-- ==========================================================================================


-- ==========================================================================================
-- DTOs for DeclareLostItem workflow
-- ==========================================================================================



-- ----------------------------------------------------------------------------
-- DTO for CreateAttributeRefForm
-- ----------------------------------------------------------------------------




data CreateAttributeRefForm = CreateAttributeRefForm {
        attrName :: String
    ,   attrDescpt :: String
    ,   attrValues :: [String]
    ,   attrUnits :: [String]
    ,   attrRelatedCats :: [(String, String)]
    } deriving (Generic, Show)

instance ToJSON CreateAttributeRefForm 

instance FromJSON CreateAttributeRefForm

-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedAttributeRef :: CreateAttributeRefForm -> UnvalidatedAttributeRef
toUnvalidatedAttributeRef  CreateAttributeRefForm{..}= 
    UnvalidatedAttributeRef {
            uattributeName        = attrName
        ,   uattributeDescription = attrDescpt
        ,   uattributesValues     = attrValues
        ,   uattributeUnits       = attrUnits
        ,   urelatedCategories    = attrRelatedCats
        }
        


-------------------------------------------------------------------------------
-- DTO for AttributeRefCreated Event
-- ----------------------------------------------------------------------------



data AttributeRefCreatedDto = AttributeRefCreatedDto {
        code :: String
    ,   name :: String
    ,   description :: String
    ,   values :: [String]
    ,   units :: [String]
    ,   relatedCats :: [(String, String)]
    } deriving (Generic, Show)


instance ToJSON AttributeRefCreatedDto

instance FromJSON AttributeRefCreatedDto

-- Helper functions for converting from / to domain as well as to other states

fromAttributeRefCreated :: AttributeRefCreated -> AttributeRefCreatedDto
fromAttributeRefCreated = 
    AttributeRefCreatedDto
        <$> code
        <*> name 
        <*> descpt 
        <*> values
        <*> units 
        <*> relatedCatgrs
    where code = uwrpAttrCd . attributeRefCode
          name = uwrpAttrNm . attributeRefName
          descpt = uwrpShrtDescpt . attributeRefDescription
          values = fmap uwrpAttrVal . attributeRefValues
          units = fmap uwrpAttrUnt . attributeRefUnits
          relatedCatgrs = 
            fmap ( \(catId, catType ) 
                        -> (uwrpCatgrId catId, uwpCatgrCd catType)) 
                    . attributeRefRelatedCategories


----


toDomain1 :: AttributeRefCreatedDto -> Either ErrorMessage AttributeRef
toDomain1 dto = do
    cd <- crtAttrCd . code $ dto
    nm <- crtAttrNm . name $ dto
    descpt <- crtShrtDescpt . description $ dto
    vals <-  traverse crtAttrVal . values $ dto
    units <- traverse crtAttrUnt . units $ dto
    refCatgrs <- traverse toPairCatIdandCatCd . relatedCats $ dto
    return 
        AttributeRef {
                attributeRefCode              = cd
            ,   attributeRefName              = nm
            ,   attributeRefDescription       = descpt
            ,   attributeRefValues            = vals
            ,   attributeRefUnits             = units
            ,   attributeRefRelatedCategories = refCatgrs
            }
    where toPairCatIdandCatCd (strid, strType) = 
            do id <- crtCatgrId strid
               typ <- crtCatgrCd strType
               return (id, typ)


-- ----------------------------------------------------------------------------
-- DTO for WorkflowError 
-- ----------------------------------------------------------------------------

--- 

newtype CreateAttributeRefEventDto = 
        CR AttributeRefCreatedDto deriving (Generic, Show)

instance ToJSON CreateAttributeRefEventDto

---

type CreateAttributeRefEventResponse = Map String CreateAttributeRefEventDto

instance ToJSONKey CreateAttributeRefEventResponse

---

type RespCrtAttrRefWorkflow = [CreateAttributeRefEventResponse] 


fromCrtAttrEvtDomain :: CreateAttributeEvent -> CreateAttributeRefEventResponse
fromCrtAttrEvtDomain evt = 
    case evt of
        AttributeRefCreated attr ->
            let key = "createdattributeref"
                val = fromAttributeRefCreated attr
            in  singleton key (CR val)
        