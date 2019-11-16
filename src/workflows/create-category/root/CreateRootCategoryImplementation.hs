{-# LANGUAGE RecordWildCards #-}

module CreateRootCategoryImplementation where

import CommonCompoundTypes
    (AttributeRef (..),
    Category (..),
    CategoryInfo (..),
    EnablementStatus (..),
    AddedSubCategory (..),
    toEnblmntStatus,
    toValidatedSubCatgrs,
    checkIsSubAndEnabled,
    createCategoryCreatedEvt,
    createSubCategoryAddedEvt
    )
import CommonSimpleTypes
import CreateCategoryCommonPublicTypes
import CreateRootCategoryPublicTypes
import Data.Either.Combinators 
    (mapLeft)
import Data.Set 
    (Set, toList, fromList)
import Util 
    (singleton)





-- ==========================================================================================
-- This file contains the initial implementation for the createRootCategory workflow
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




--- Referenced subcategories data validation (a referenced sub category must be both not Root and not Disabled)
---
---


type RefSubCategoryValidationError = String



type CheckRefSubCatgrValid =
  UnvalidatedCategoryId ->
  Either RefSubCategoryValidationError CategoryId




data ValidatedRootCategory
  = ValidatedRootCategory
      { vrootCategoryId :: CategoryId,
        vrootCategoryCode :: CategoryCode,
        vrootCategoryEnablementStatus :: EnablementStatus,
        vrootcategoryDescription :: LongDescription,
        vrootCatgrRelatedSubCatgrs :: Set CategoryId
      }
  deriving (Eq, Ord, Show)





-- ----------------------------------------------------------------------------
-- Verify referred sub categories are not either Root or already have a parent step
-- ----------------------------------------------------------------------------





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




validateUnvalidatedCategory :: UnvalidatedRootCategory
                                -> UnvalidatedCategoryId
                                -> Either ValidationError ValidatedRootCategory
validateUnvalidatedCategory ucatgr ucatgrId =
  ValidatedRootCategory 
    <$> catgrId 
    <*> catgrCd 
    <*> enblmntStatus 
    <*> descpt
    <*> subCatgrs
  where
    catgrId = toCategoryId ucatgrId
    catgrCd = toCategoryCode . urootCategoryCode $ ucatgr
    enblmntStatus = toEnblmntStatus . urootCategoryEnablement $ ucatgr
    descpt = toLongDescpt . urootCategoryDescription $ ucatgr
    subCatgrs = toValidatedSubCatgrs . urootCatgrRelatedsubCatgrs $ ucatgr




-- ----------------------------------------------------------------------------
-- Verify reffered sub categories are not either Root or already have 
-- a parent step
-- ----------------------------------------------------------------------------




--- TODO: I should probably use a foldr here ???

checkRefSubCatgrsValid :: [Category] 
                            -> ValidatedRootCategory 
                            -> Either DomainError [CategoryId]
checkRefSubCatgrsValid catgrs =
  traverse (checkRefSubCatgrValid catgrs) . toList . vrootCatgrRelatedSubCatgrs
  where
    checkRefSubCatgrValid :: [Category] 
                                -> CategoryId 
                                -> Either DomainError CategoryId
    checkRefSubCatgrValid cats catId =
      let singletonCatgr = filter (\cat -> toCatgrId cat == catId) cats
       in case singletonCatgr of
            [catgr] ->
              checkIsSubAndEnabled catId catgr
            _ ->
              Left $ DomainError "referenced sub category not found"
      where
        toCatgrId (RootCategory catgrInfo) = categoryId catgrInfo
        toCatgrId (SubCategory catgrInfo _) = categoryId catgrInfo





-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------





createRootCategory :: ValidatedRootCategory -> Category
createRootCategory ValidatedRootCategory {..} =
  let catgrInfo =
        CategoryInfo
          { categoryId = vrootCategoryId,
            categoryCode = vrootCategoryCode,
            categoryEnablementStatus = vrootCategoryEnablementStatus,
            categoryDescription = vrootcategoryDescription,
            categoryRelatedSubCategories = vrootCatgrRelatedSubCatgrs
          }
   in RootCategory catgrInfo





-- ----------------------------------------------------------------------------
-- Create Events step
-- ----------------------------------------------------------------------------





createEvents :: Category -> [CreateCategoryEvent]
createEvents cat =
  let rtCatCrtedEvt = singleton . CategoryCreated . createCategoryCreatedEvt $ cat
      subCatsAddedEvt = singleton . SubCategoriesAdded . createSubCategoryAddedEvt $ cat
   in case head subCatsAddedEvt of
        SubCategoriesAdded [] -> rtCatCrtedEvt
        _ -> mconcat [rtCatCrtedEvt, subCatsAddedEvt]






-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --
-- Overall workflow --
-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --






createRootCatgory :: [Category]
                    -> UnvalidatedRootCategory
                    -> UnvalidatedCategoryId
                    -> Either WorkflowError [CreateCategoryEvent]
createRootCatgory referredSubCatgrs
                  unvalidatedCategory
                  unValidatedCatgrId =
    do  validatedCatgr <- mapValidationErr 
                                $ validateUnvalidatedCategory unvalidatedCategory
                                                              unValidatedCatgrId    
        _ <- mapDomainErr $ checkRefSubCatgrsValid referredSubCatgrs validatedCatgr
        createdCatgr <- return $ createRootCategory validatedCatgr      
        return $ createEvents createdCatgr
