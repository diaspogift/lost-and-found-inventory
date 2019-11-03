{-# LANGUAGE RecordWildCards #-}


module CreateRootCategoryImplementation where

import CommonSimpleTypes
import CommonCompoundTypes
import CreateCategoryCommonPublicTypes
import CreateRootCategoryPublicTypes


import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set hiding (singleton, null, filter)
import Util
import Data.Either.Combinators

import Data.UUID.V4
import Data.UUID hiding (null) -- Internal




-- ==========================================================================================
-- This file contains the initial implementation for the createRootCategory workflow
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


--- Referenced subcategories data validation (a referenced sub category must be both not Root and not Disabled)
--- 
---
---


type RefSubCategoryValidationError = String

type CheckRefSubCatgrValid = 
    UnvalidatedCategoryId
    -> Either RefSubCategoryValidationError CategoryId


--- Validated Category

data ValidatedRootCategory = ValidatedRootCategory {
        vrootCategoryId                 :: CategoryId
    ,   vrootCategoryCode               :: CategoryCode
    ,   vrootCategoryEnablementStatus   :: EnablementStatus
    ,   vrootcategoryDescription        :: LongDescription
    ,   vrootCatgrRelatedSubCatgrs      :: Set CategoryId
    } deriving (Eq, Ord, Show)



type ValidateUnvalidatedRootCategory =
  UnvalidatedRootCategory 
  -> UnvalidatedCategoryId 
  -> Either ValidationError ValidatedRootCategory





-- ----------------------------------------------------------------------------
-- Verify reffered sub categories are not either Root or already have a parent step
-- ----------------------------------------------------------------------------



type CheckRefSubCatgrsValid = 
  [Category]
  -> ValidatedRootCategory
  -> Either DomainError [CategoryId]





-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------



type CreateRootCategory =
  ValidatedRootCategory -> Category



-- ----------------------------------------------------------------------------
-- Create events step
-- ----------------------------------------------------------------------------


type CreateEvents =
  Category -> [CreateCategoryEvent]






-- ==========================================================================================
-- Section 2 : Implementation
-- ==========================================================================================






-- ----------------------------------------------------------------------------
-- Validation step
-- ----------------------------------------------------------------------------



validateUnvalidatedCategory :: ValidateUnvalidatedRootCategory
validateUnvalidatedCategory uCatgr uCatgrId = 
    ValidatedRootCategory <$> id <*> code <*> enblmntStatus <*> descpt <*> subCatgrs
      where 
        id = toCatId uCatgrId
        code = (toCatCd . urootCategoryCode) uCatgr
        enblmntStatus = (toEnblmntStatus . urootCategoryEnablement) uCatgr
        descpt = (toDescpt . urootCategoryDescription) uCatgr
        subCatgrs = (toValidatedSubCatgrs . urootCatgrRelatedsubCatgrs) uCatgr
        
      


--- Helper functions for validateUnvalidatedCategory start

toCatId :: String -> Either ValidationError CategoryId
toCatId = mapLeft ValidationError . crtCatgrId
         
toCatCd :: String -> Either ValidationError CategoryCode
toCatCd = mapLeft ValidationError . crtCatgrCd      
  
toEnblmntStatus :: String -> Either ValidationError EnablementStatus
toEnblmntStatus str 
    | str == "enabled" = Right $ Enabled  "Enabled at creation time"
    | str == "disabled" = Right . Disabled $ "Disabled at creation time"
    | otherwise = mapLeft ValidationError . Left $ "enablement status is either enabled or disabled"

toDescpt :: String -> Either ValidationError LongDescription
toDescpt = mapLeft ValidationError . crtLgDescpt  
  
toValidatedSubCatgrs :: 
    [String] -> Either ValidationError (Set CategoryId)
toValidatedSubCatgrs  = 
    fmap fromList . traverse (mapLeft ValidationError . crtCatgrId) 
    





-- ----------------------------------------------------------------------------
-- Verify reffered sub categories are not either Root or already have a parent step
-- ----------------------------------------------------------------------------


--- TODO: I should probably use a fold here 
--- TODO: I should probably use a fold here 
--- TODO: I should probably use a fold here 

 

checkRefSubCatgrsValid :: CheckRefSubCatgrsValid 
checkRefSubCatgrsValid catgrs = 

    traverse (checkRefSubCatgrValid catgrs) . toList . vrootCatgrRelatedSubCatgrs
    where checkRefSubCatgrValid :: [Category] -> CategoryId -> Either DomainError CategoryId
          checkRefSubCatgrValid cats catId = 

            let singletonCatgr = filter (\cat ->  toCatgrId cat == catId) cats

            in case singletonCatgr of
                [catgr] -> 
                    checkIsSubAndEnabled catId catgr
                _ -> 
                    Left $ DomainError "referenced sub category not found"
                
            where   toCatgrId (RootCategory catgrInfo) = categoryId catgrInfo
                    toCatgrId (SubCategory catgrInfo _) = categoryId catgrInfo
      
                    
checkIsSubAndEnabled :: CategoryId -> Category -> Either DomainError CategoryId                 
checkIsSubAndEnabled catId (RootCategory _) =  
    Left $ DomainError "a root category cannot be sub category"
checkIsSubAndEnabled catId (SubCategory _ (Just _)) =
    Left $ DomainError "the sub category is already sub for: TODO "
checkIsSubAndEnabled catId (SubCategory CategoryInfo {categoryEnablementStatus = enblmnt } Nothing) =
    case enblmnt of
        Disabled reason -> Left . DomainError $ "the sub category is disabled for: " <> reason
        Enabled info -> return catId
                            


-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------




createRootCategory :: ValidatedRootCategory -> Category
createRootCategory  ValidatedRootCategory {..} =
   let catgrInfo = 
        CategoryInfo {
            categoryId                      = vrootCategoryId
        ,   categoryCode                    = vrootCategoryCode
        ,   categoryEnablementStatus        = vrootCategoryEnablementStatus
        ,   categoryDescription             = vrootcategoryDescription
        ,   categoryRelatedSubCategories    = vrootCatgrRelatedSubCatgrs
        }
    in RootCategory catgrInfo

    
  
-- ----------------------------------------------------------------------------
-- Create Events step
-- ----------------------------------------------------------------------------



createEvents :: Category -> [CreateCategoryEvent]
createEvents  cat =
        let rtCatCrtedEvt = singleton . CategoryCreated . createCategoryCreatedEvt $ cat 
            subCatsAddedEvt = singleton . SubCategoriesAdded . createSubCategoryAddedEvt $ cat
        in 
            case head subCatsAddedEvt of 
                SubCategoriesAdded [] -> rtCatCrtedEvt
                _ -> concat [rtCatCrtedEvt, subCatsAddedEvt]

  


--- Helper functions 
---
---

createCategoryCreatedEvt :: Category -> CategoryCreated
createCategoryCreatedEvt (RootCategory catgrInfo) = 
    RootCategory $ catgrInfo { categoryRelatedSubCategories = fromList []}

createSubCategoryAddedEvt :: Category -> [AddedSubCategory]
createSubCategoryAddedEvt (RootCategory CategoryInfo { categoryId = id, categoryRelatedSubCategories = subCatgrs}) =
    fmap (AddedSubCategory id) . toList $ subCatgrs
    






-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --
                         -- Overall workflow --
-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --





createRootCatgory ::
  [Category] 
  -> UnvalidatedRootCategory 
  -> UnvalidatedCategoryId 
  -> Either WorkflowError [CreateCategoryEvent]

createRootCatgory  
  referredSubCatgrs         -- Dependency       
  unvalidatedCategory           -- Input
  unValidatedCatgrId =          -- Input
    do  
        -- Validation step
        validatedCatgr 
            <- mapLeft 
                Validation $
                    validateUnvalidatedCategory
                        unvalidatedCategory
                        unValidatedCatgrId

        -- Verify that referred sub categories have their RootStatus set to Sub and Enablement Status set to enabled
        _ <- mapLeft 
                Domain $ 
                    checkRefSubCatgrsValid
                        referredSubCatgrs
                        validatedCatgr     

        -- Creation step
        createdCatgr 
            <- return $ 
                    createRootCategory
                        validatedCatgr

        -- Events creation step
        return $ createEvents createdCatgr
                

          








