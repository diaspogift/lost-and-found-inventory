module CreateAttributeImplementation where

import CommonSimpleTypes
import CommonCompoundTypes
import CreateAttributePublicTypes


import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set hiding (singleton, null)
import Util
import Data.Either.Combinators

import Data.UUID.V4
import Data.UUID hiding (null) -- Internal




-- ==========================================================================================
-- This file contains the initial implementation for the createAttrinute workflow
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

type RefCategoryValidationError = String


--- Validated Attribute

data ValidatedAttributeRef = ValidatedAttributeRef {
      vattrCodeRef              :: AttributeCode
    , vattrNameRef              :: AttributeName
    , vattrDescriptionRef       :: ShortDescription
    , vattrValueRefss            :: [AttributeValue]
    , vattrUnitRefss             :: [AttributeUnit]
    , vrelatedCategoriesRefs    :: [(CategoryId, CategoryCode)]
    } deriving (Eq, Ord, Show)



type ValidateUnvalidatedAttributeRef =
  UnvalidatedAttributeRef -> UnvalidatedAttributeCode -> Either ValidationError ValidatedAttributeRef




-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------



type CreateAttributeRef =
  ValidatedAttributeRef -> AttributeRef



-- ----------------------------------------------------------------------------
-- Create events step
-- ----------------------------------------------------------------------------


type CreateEvents =
  AttributeRef -> [CreateAttributeEvent]


-- ==========================================================================================
-- Section 2 : Implementation
-- ==========================================================================================






-- ----------------------------------------------------------------------------
-- Validation step
-- ----------------------------------------------------------------------------



validateUnvalidatedAttributeRef :: ValidateUnvalidatedAttributeRef
validateUnvalidatedAttributeRef uAttrRef uUuidCd = 
    ValidatedAttributeRef <$> code <*> name <*> desc <*> values <*> units <*> refCats
      where 
        code = toAttrCdRef uUuidCd
        name = (toAttrNm . uattrNm) uAttrRef
        desc = (toAttrDescpt . uattrDescpt) uAttrRef
        values = traverse toAttrVal $ uattrVals uAttrRef
        units = traverse toAttrUnt $ uattrUnts uAttrRef
        refCats = traverse toAttrRefCatgrs $ urelatedCatgrs uAttrRef
      
---


---

--- Helper functions for validateUnvalidatedAttributeRef

         
toAttrCdRef :: String -> Either ValidationError AttributeCode
toAttrCdRef str = 
  mapLeft ValidationError $ crtAttrCd str     

toAttrNm :: String -> Either ValidationError AttributeName
toAttrNm str = 
    mapLeft ValidationError $ crtAttrNm str     

toAttrDescpt :: String -> Either ValidationError ShortDescription
toAttrDescpt str = 
  mapLeft ValidationError $ crtShrtDescpt str    

toAttrVal :: String -> Either ValidationError AttributeValue
toAttrVal str = 
  mapLeft ValidationError $ crtAttrVal str  
  
toAttrUnt :: String -> Either ValidationError AttributeUnit
toAttrUnt str = 
  mapLeft ValidationError $ crtAttrUnt str 
      

toAttrRefCatgrs :: (String, String) -> Either ValidationError (CategoryId, CategoryCode)
toAttrRefCatgrs (catId, catType) = 
    do  id <- mapLeft ValidationError $ crtCatgrId catId
        typ <- mapLeft ValidationError $ crtCatgrCd catType
        return (id, typ)





-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------




createAttrinuteRef :: ValidatedAttributeRef -> AttributeRef
createAttrinuteRef  =
  AttributeRef 
    <$> vattrCodeRef
    <*> vattrNameRef
    <*> vattrDescriptionRef
    <*> vattrValueRefss
    <*> vattrUnitRefss
    <*> vrelatedCategoriesRefs



-- ----------------------------------------------------------------------------
-- Create Events step
-- ----------------------------------------------------------------------------



createEvents :: AttributeRef -> [CreateAttributeEvent]
createEvents  =
        singleton . AttributeRefCreated . createAttrRefCreatedEvt 

  


--- Helper functions 
---
---

createAttrRefCreatedEvt :: AttributeRef -> AttributeRef
createAttrRefCreatedEvt declaredLostItem = declaredLostItem






-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --
                         -- Overall workflow --
-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --





createAttributeReference ::
  UnvalidatedAttributeRef
  -> UnvalidatedAttributeCode
  -> Either WorkflowError [CreateAttributeEvent]

createAttributeReference                 
  unvalidatedAttributeRef           -- Input
  unValidatedlostItemUuid =         -- Input
    do  
        -- Validation step
        validatedAttrRef 
            <- mapLeft 
                Validation $
                    validateUnvalidatedAttributeRef
                        unvalidatedAttributeRef
                        unValidatedlostItemUuid

        -- Creation step
        createdAttrRef 
            <- return $ 
                    createAttrinuteRef
                        validatedAttrRef

        -- Events creation step
        return $ createEvents createdAttrRef
                

          








