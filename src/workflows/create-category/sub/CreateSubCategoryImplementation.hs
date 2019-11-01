module CreateSubCategoryImplementation where

import CommonSimpleTypes
import CommonCompoundTypes
import CreateSubCategoryPublicTypes


import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set hiding (singleton, null, filter)
import Util
import Data.Either.Combinators

import Data.UUID.V4
import Data.UUID hiding (null) -- Internal




-- ==========================================================================================
-- This file contains the initial implementation for the createSubCategory workflow
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


--- Referenced subcategories data validation (a referenced sub category must be both not Sub and not Disabled)
--- 
---
---


type RefSubCategoryValidationError = String

type CheckRefSubCatgrValid = 
    UnvalidatedSubCategoryId
    -> Either RefSubCategoryValidationError CategoryId


--- Validated Category

data ValidatedSubCategory = ValidatedSubCategory {
        vsubCategoryId                  :: CategoryId
    ,   vsubCategoryCode                :: CategoryCode
    ,   vsubCategoryParentIdCd          :: Maybe (CategoryId, CategoryCode)
    ,   vsubCategoryEnablementStatus    :: EnablementStatus
    ,   vsubCategoryDescription            :: LongDescription
    ,   vsubCatgrRelatedSubCatgrs       :: Set CategoryId
    } deriving (Eq, Ord, Show)



type ValidateUnvalidatedSubCategory =
  UnvalidatedSubCategory 
  -> UnvalidatedSubCategoryId 
  -> Either ValidationError ValidatedSubCategory





-- ----------------------------------------------------------------------------
-- Verify reffered sub categories are not either Root or already have a parent step
-- ----------------------------------------------------------------------------



type CheckRefSubCatgrsValid = 
  [Category]
  -> ValidatedSubCategory
  -> Either DomainError [CategoryId]





-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------



type CreateSubCategory =
  ValidatedSubCategory -> Maybe Category -> Category



-- ----------------------------------------------------------------------------
-- Create events step
-- ----------------------------------------------------------------------------


type CreateEvents =
  Category -> [CreateSubCategoryEvent]






-- ==========================================================================================
-- Section 2 : Implementation
-- ==========================================================================================






-- ----------------------------------------------------------------------------
-- Validation step
-- ----------------------------------------------------------------------------



validateUnvalidatedCategory :: ValidateUnvalidatedSubCategory
validateUnvalidatedCategory uCatgr uCatgrId = 
    do 
        id <- toCatId uCatgrId
        code <- toCatCd . usubCategoryCode $ uCatgr
        parntIdCd <- toMaybePrntIdCd . usubCategoryParentIdandCd $ uCatgr
        enblmntStatus <- (toEnblmntStatus . usubCatgrEnablementStatus) uCatgr
        descpt <- toDescpt . usubCategoryDescription $ uCatgr
        subCatgrs <- toValidatedSubCatgrs . usubCatgrRelatedsubCatgrs $ uCatgr

        return $
            ValidatedSubCategory {
                    vsubCategoryId                = id  
                ,   vsubCategoryCode              = code 
                ,   vsubCategoryParentIdCd        = parntIdCd    
                ,   vsubCategoryEnablementStatus  = enblmntStatus
                ,   vsubCategoryDescription          = descpt
                ,   vsubCatgrRelatedSubCatgrs     = subCatgrs  
                }
        
            
      

--- Helper functions for validateUnvalidatedCategory start

toMaybePrntIdCd :: (String, String) -> Either ValidationError (Maybe (CategoryId, CategoryCode))
toMaybePrntIdCd (prntId, prntCd)
            | null prntId && null prntCd =
                return Nothing
            | otherwise = 
                do prntCatId <- mapLeft ValidationError $ crtCatgrId prntId
                   prntCatCd <- mapLeft ValidationError $  crtCatgrCd prntCd
                   return . Just $ (prntCatId, prntCatCd)

toCatId :: String -> Either ValidationError CategoryId
toCatId = mapLeft ValidationError . crtCatgrId
         
toCatCd :: String -> Either ValidationError CategoryCode
toCatCd = mapLeft ValidationError . crtCatgrCd      
  
toEnblmntStatus :: String -> Either ValidationError EnablementStatus
toEnblmntStatus str 
    | str == "enabled" = Right . Enabled $ "Enabled at creation time"
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
    traverse (checkRefSubCatgrValid catgrs) . toList . vsubCatgrRelatedSubCatgrs
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
                    
                where verifyNotRootAndNotSub Category { categoryEnablementStatus = es, categoryRootStatus = rs, categoryId = cid } =
                        case es of 
                            Disabled reason ->
                                Left . DomainError $ 
                                    "referenced sub category with id : " 
                                    ++ uwrpCatgrId cid  ++ " is disabled for reason: " ++ reason 
                            Enabled _ ->
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
-- Vefify (if present) that referred parent category **exists** and is not part of
-- the subs categories specified.
-- Its existence will be determined by a database lookup result
-- ----------------------------------------------------------------------------


checkRefPrntCatgrValid :: Maybe Category -> ValidatedSubCategory -> Either DomainError ValidatedSubCategory
checkRefPrntCatgrValid maybePrntCatgr vSubCatgr =
    case maybePrntCatgr of
        Just prntCatgr ->
            case filter (parentCatgrIn prntCatgrId) subCatgrIds of
                [] -> return vSubCatgr
                _ -> Left . DomainError $ "parent - sub categories recursion not alowed"
            where   parentCatgrIn catId subCatId =  catId == subCatId
                    subCatgrIds = toList . vsubCatgrRelatedSubCatgrs $ vSubCatgr
                    prntCatgrId = categoryId prntCatgr
        Nothing -> return vSubCatgr {vsubCategoryParentIdCd = Nothing}





-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------




createSubCategory :: ValidatedSubCategory -> Category
createSubCategory vSubCatgr =
  case vsubCategoryParentIdCd vSubCatgr of
      Just (prtCatId, prtCatCode) -> 

        Category {
            categoryId =        vsubCategoryId vSubCatgr
        ,   categoryCode =      vsubCategoryCode vSubCatgr
        ,   categoryRootStatus  = Sub . Just $ ParentInfo prtCatId prtCatCode
        ,   categoryEnablementStatus = vsubCategoryEnablementStatus vSubCatgr
        ,   categoryDescription = vsubCategoryDescription vSubCatgr
        ,   categoryRelatedSubCategories = vsubCatgrRelatedSubCatgrs vSubCatgr
        }

      Nothing -> 
        
        Category {
            categoryId = vsubCategoryId vSubCatgr
        ,   categoryCode = vsubCategoryCode vSubCatgr
        ,   categoryRootStatus  = Sub Nothing
        ,   categoryEnablementStatus = vsubCategoryEnablementStatus vSubCatgr
        ,   categoryDescription = vsubCategoryDescription vSubCatgr
        ,   categoryRelatedSubCategories = vsubCatgrRelatedSubCatgrs vSubCatgr
        }




    
  
-- ----------------------------------------------------------------------------
-- Create Events step
-- ----------------------------------------------------------------------------



createEvents :: Category -> [CreateSubCategoryEvent]
createEvents  cat =
        let rtCatCrtedEvt = singleton . SubCategoryCreated . createCategoryCreatedEvt $ cat 
            subCatsAddedEvt = singleton . SubCategoriesAdded . createSubCategoryAddedEvt $ cat
        in 
            case head subCatsAddedEvt of 
                SubCategoriesAdded [] -> rtCatCrtedEvt
                _ -> concat [rtCatCrtedEvt, subCatsAddedEvt]

  


--- Helper functions 
---
---

createCategoryCreatedEvt :: Category -> SubCategoryCreated
createCategoryCreatedEvt createdCategory = 
    createdCategory { categoryRelatedSubCategories = fromList []}

createSubCategoryAddedEvt :: Category -> [AddedSubCategory]
createSubCategoryAddedEvt Category { categoryId = id, categoryRelatedSubCategories = subCatgrs} =
    fmap (AddedSubCategory id) . toList $ subCatgrs
    






-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --
                         -- Overall workflow --
-- ---------------------------------------------------------------------------- --
-- ---------------------------------------------------------------------------- --





createSubCatgory ::
  [Category] 
  -> Maybe Category
  -> UnvalidatedSubCategory 
  -> UnvalidatedSubCategoryId 
  -> Either WorkflowError [CreateSubCategoryEvent]

createSubCatgory  
  referredSubCatgrs             -- Input   
  referencedParentCatgr         -- Input 
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
                        
        -- Vefify (if present) that the referred parent category **exists** and is not part of the subs categories specified
        validatedCatgrN
            <- mapLeft 
                Domain $
                    checkRefPrntCatgrValid 
                        referencedParentCatgr
                        validatedCatgr

        -- Creation step
        createdCatgr 
            <- return $ 
                    createSubCategory
                        validatedCatgrN

        -- Events creation step
        return $ createEvents createdCatgr
                

          








