
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
        name :: String
    ,   description :: String
    ,   values :: [String]
    ,   units :: [String]
    ,   relatedCats :: [(String, String)]
    } deriving (Generic, Show)

instance ToJSON CreateAttributeRefForm 

instance FromJSON CreateAttributeRefForm

-- Helper functions for converting from / to domain as well as to other states


toUnvalidatedAttributeRef :: CreateAttributeRefForm -> UnvalidatedAttributeRef
toUnvalidatedAttributeRef  CreateAttributeRefForm{..}= 
    UnvalidatedAttributeRef {
            uattrNm = name
        ,   uattrDescpt = description
        ,   uattrVals = values
        ,   uattrUnts = units
        ,   urelatedCatgrs = relatedCats
        }
        


-------------------------------------------------------------------------------
-- DTO for AttributeRefCreated Event
-- ----------------------------------------------------------------------------



data AttributeRefCreatedDto = AttributeRefCreatedDto {
        attrCode :: String
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
    where code = uwrpAttrCd . attrCodeRef
          name = uwrpAttrNm . attrNameRef
          descpt = uwrpShrtDescpt . attrDescriptionRef
          values = (fmap uwrpAttrVal ) . attrValueRefs
          units = (fmap uwrpAttrUnt) . attrUnitRefs
          relatedCatgrs = fmap ( \(catId, catType )-> (uwrpCatgrId catId, uwpCatgrCd catType)) . relatedCategoriesRefs



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
        