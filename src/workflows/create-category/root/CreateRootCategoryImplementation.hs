module CreateRootCategoryImplementation where

import CommonSimpleTypes
import CommonCompoundTypes
import CreateRootCategoryPublicTypes


import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set hiding (singleton, null)
import Util
import Data.Either.Combinators

import Data.UUID.V4
import Data.UUID hiding (null) -- Internal




-- ==========================================================================================
-- This file contains the initial implementation for the createCategory workflow
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


--- Referenced subcategories data validation

type RefSubCategoryValidationError = String

type CheckRefSubCatgrValid = 
    UnvalidatedRootCategoryId
    -> Either RefSubCategoryValidationError CategoryId


--- Validated Category

data ValidatedCategory = ValidatedCategory {
        vcategoryId          :: CategoryId
    ,   vcategoryCode        :: CategoryCode
    ,   vrootStatus          :: RootStatus
    ,   venablementStatus    :: EnablementStatus
    ,   vcategoryDesc        :: LongDescription
    ,   vsubCategories       :: Set CategoryId
    } deriving (Eq, Ord, Show)



type ValidateUnvalidatedRootCategory =
  CheckRefSubCatgrValid
  -> UnvalidatedRootCategory 
  -> UnvalidatedRootCategoryId 
  -> Either ValidationError ValidatedCategory




-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------



type CreateRootCategory =
  ValidatedCategory -> Category



-- ----------------------------------------------------------------------------
-- Create events step
-- ----------------------------------------------------------------------------


type CreateEvents =
  Category -> [CreateRootCategoryEvent]


-- ==========================================================================================
-- Section 2 : Implementation
-- ==========================================================================================






-- ----------------------------------------------------------------------------
-- Validation step
-- ----------------------------------------------------------------------------



validateUnvalidatedCategory :: ValidateUnvalidatedRootCategory
validateUnvalidatedCategory checkRefSubCatgrValid uCatgr uCatgrId = 
    ValidatedCategory <$> id <*> code <*> rootStatus <*> enblmntStatus <*> descpt <*> subCatgrs
      where 
        id = toCatId uCatgrId
        code = (toCatCd . ucatCd) uCatgr
        rootStatus = pure Root
        enblmntStatus = (toEnblmntStatus . uEnblmnt) uCatgr
        descpt = (toDescpt . udescpt) uCatgr
        subCatgrs = (toValidatedSubCatgrs checkRefSubCatgrValid . usubCatgrs) uCatgr
        
      

---             Helper functions for validateUnvalidatedCategory start          ---

toCatId :: String -> Either ValidationError CategoryId
toCatId = mapLeft ValidationError . crtCatgrId
         
toCatCd :: String -> Either ValidationError CategoryCode
toCatCd = mapLeft ValidationError . crtCatgrCd      
  
toEnblmntStatus :: String -> Either ValidationError EnablementStatus
toEnblmntStatus str 
    | str == "enabled" = Right Enabled
    | str == "disabled" = Right . Disabled $ "Disabled at creation time"
    | otherwise = mapLeft ValidationError . Left $ "enablement status is either enabled or disabled"

toDescpt :: String -> Either ValidationError LongDescription
toDescpt = mapLeft ValidationError . crtLgDescpt  
  
toValidatedSubCatgrs :: 
    CheckRefSubCatgrValid -> [String] -> Either ValidationError (Set CategoryId)
toValidatedSubCatgrs checkRefSubCatgrValid  =  
    fmap fromList . mapLeft ValidationError . traverse checkRefSubCatgrValid 

---             Helper functions for validateUnvalidatedCategory end           ---






-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------




createRootCategory :: ValidatedCategory -> Category
createRootCategory  =
  Category 
    <$> vcategoryId
    <*> vcategoryCode
    <*> vrootStatus
    <*> venablementStatus
    <*> vcategoryDesc
    <*> vsubCategories

  
-- ----------------------------------------------------------------------------
-- Create Events step
-- ----------------------------------------------------------------------------



createEvents :: Category -> [CreateRootCategoryEvent]
createEvents  cat =
        let rtCatCrtedEvt = singleton . RootCategoryCreated . createCategoryCreatedEvt $ cat 
            subCatsAddedEvt = singleton . SubCategoriesAdded . createSubCategoryAddedEvt $ cat
        in 
            case head subCatsAddedEvt of 
                SubCategoriesAdded [] -> rtCatCrtedEvt
                _ -> concat [rtCatCrtedEvt, subCatsAddedEvt]

  


--- Helper functions 
---
---

createCategoryCreatedEvt :: Category -> RootCategoryCreated
createCategoryCreatedEvt createdCategory = createdCategory

createSubCategoryAddedEvt :: Category -> [AddedSubCategory]
createSubCategoryAddedEvt Category { categoryId = id, subCategories = subCatgrs} =
    fmap (AddedSubCategory id) . toList $ subCatgrs
    






-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --
                         -- Overall workflow --
-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --





createRootCatgory ::
  CheckRefSubCatgrValid
  -> UnvalidatedRootCategory 
  -> UnvalidatedRootCategoryId 
  -> Either WorkflowError [CreateRootCategoryEvent]

createRootCatgory  
  checkRefSubCatgrValid         -- Dependency       
  unvalidatedCategory           -- Input
  unValidatedCatgrId =          -- Input
    do  
        -- Validation step
        validatedCatgr 
            <- mapLeft 
                Validation $
                    validateUnvalidatedCategory
                        checkRefSubCatgrValid
                        unvalidatedCategory
                        unValidatedCatgrId

        -- Creation step
        createdCatgr 
            <- return $ 
                    createRootCategory
                        validatedCatgr

        -- Events creation step
        return $ createEvents createdCatgr
                

          








