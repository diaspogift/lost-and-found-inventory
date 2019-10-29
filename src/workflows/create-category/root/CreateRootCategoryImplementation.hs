module CreateRootCategoryImplementation where

import CommonSimpleTypes
import CommonCompoundTypes
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
    UnvalidatedRootCategoryId
    -> Either RefSubCategoryValidationError CategoryId


--- Validated Category

data ValidatedRootCategory = ValidatedRootCategory {
        vcategoryId          :: CategoryId
    ,   vcategoryCode        :: CategoryCode
    ,   vrootStatus          :: RootStatus
    ,   venablementStatus    :: EnablementStatus
    ,   vcategoryDesc        :: LongDescription
    ,   vsubCategories       :: Set CategoryId
    } deriving (Eq, Ord, Show)



type ValidateUnvalidatedRootCategory =
  UnvalidatedRootCategory 
  -> UnvalidatedRootCategoryId 
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
  Category -> [CreateRootCategoryEvent]






-- ==========================================================================================
-- Section 2 : Implementation
-- ==========================================================================================






-- ----------------------------------------------------------------------------
-- Validation step
-- ----------------------------------------------------------------------------



validateUnvalidatedCategory :: ValidateUnvalidatedRootCategory
validateUnvalidatedCategory uCatgr uCatgrId = 
    ValidatedRootCategory <$> id <*> code <*> rootStatus <*> enblmntStatus <*> descpt <*> subCatgrs
      where 
        id = toCatId uCatgrId
        code = (toCatCd . ucatCd) uCatgr
        rootStatus = pure Root
        enblmntStatus = (toEnblmntStatus . uEnblmnt) uCatgr
        descpt = (toDescpt . udescpt) uCatgr
        subCatgrs = (toValidatedSubCatgrs . usubCatgrs) uCatgr
        
      

--- Helper functions for validateUnvalidatedCategory start
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
    traverse (checkRefSubCatgrValid catgrs) . toList . vsubCategories
    where checkRefSubCatgrValid :: [Category] -> CategoryId -> Either DomainError CategoryId
          checkRefSubCatgrValid cats catId =
            let ucatId = uwrpCatgrId catId
                singletonCat = filter (\cat ->  categoryId cat == catId) cats
            in case singletonCat of
                [cat] ->
                    verifyNotRootAndNotSub cat
                _ -> 
                    Left . DomainError $ 
                        "referenced sub category with id : " 
                        ++ ucatId  ++ " not found"
                    
                where verifyNotRootAndNotSub Category { enablementStatus = es, rootStatus = rs, categoryId = cid } =
                        case es of 
                            Disabled reason ->
                                Left . DomainError $ 
                                    "referenced sub category with id : " 
                                    ++ uwrpCatgrId cid  ++ " is disabled for reason: " ++ reason 
                            Enabled ->
                                case rs of
                                    Root ->
                                        Left . DomainError $
                                            "referenced sub category with id : " 
                                            ++ uwrpCatgrId cid ++ " is a root category"
                                    Sub maybeParentInfo -> 
                                        case maybeParentInfo of 
                                            Just parentInfo -> 
                                                Left . DomainError $
                                                    "referenced sub category with id : " 
                                                    ++ uwrpCatgrId cid ++ " is already a sub for: " ++ "<<<<<<<<<< TODO >>>>>>>>>>>>"
                                            Nothing -> Right cid



-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------




createRootCategory :: ValidatedRootCategory -> Category
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
createCategoryCreatedEvt createdCategory = 
    createdCategory { subCategories = fromList []}

createSubCategoryAddedEvt :: Category -> [AddedSubCategory]
createSubCategoryAddedEvt Category { categoryId = id, subCategories = subCatgrs} =
    fmap (AddedSubCategory id) . toList $ subCatgrs
    






-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --
                         -- Overall workflow --
-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --





createRootCatgory ::
  [Category] 
  -> UnvalidatedRootCategory 
  -> UnvalidatedRootCategoryId 
  -> Either WorkflowError [CreateRootCategoryEvent]

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
                

          








