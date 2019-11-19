module CreateSubCategoryImplementation where


import Common.CompoundTypes
    (AttributeRef (..),
    Category (..),
    CategoryInfo (..),
    ParentInfo (..),
    EnablementStatus (..),
    AddedSubCategory (..),
    toEnblmntStatus,
    toValidatedSubCatgrs,
    checkIsSubAndEnabled,
    createCategoryCreatedEvt,
    createSubCategoryAddedEvt
    )
import Common.SimpleTypes
import CreateCategoryCommonPublicTypes
import CreateSubCategoryPublicTypes
import Data.Either.Combinators 
    (mapLeft)
import Data.Set 
    (Set, toList, fromList)
import Util 
    (singleton)





-- ==========================================================================================
-- This file contains the initial implementation for the createSubCategory workflow
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




type RefSubCategoryValidationError = String




type CheckRefSubCatgrValid =
  UnvalidatedCategoryId ->
  Either RefSubCategoryValidationError CategoryId





data ValidatedSubCategory
  = ValidatedSubCategory
      { vsubCategoryId :: CategoryId,
        vsubCategoryCode :: CategoryCode,
        vsubCategoryParentIdCd :: Maybe (CategoryId, CategoryCode),
        vsubCategoryEnablementStatus :: EnablementStatus,
        vsubCategoryDescription :: LongDescription,
        vsubCatgrRelatedSubCatgrs :: Set CategoryId
      }
  deriving (Eq, Ord, Show)





-- ----------------------------------------------------------------------------
-- Verify reffered sub categories are not either Root or already have a parent step
-- ----------------------------------------------------------------------------
  




-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------




type CreateSubCategory =
  ValidatedSubCategory -> Maybe Category -> Category




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




validateUnvalidatedCategory :: UnvalidatedSubCategory
                            -> UnvalidatedCategoryId
                            -> Either ValidationError ValidatedSubCategory
validateUnvalidatedCategory ucatgr ucatgrId =
    do catgrId <- toCategoryId ucatgrId
       catgrCd <- toCategoryCode . usubCategoryCode $ ucatgr
       parntIdCd <- toMaybePrntIdCd . usubCategoryParentIdandCd $ ucatgr
       enblmntStatus <- toEnblmntStatus . usubCatgrEnablementStatus $ ucatgr
       descpt <- toLongDescpt . usubCategoryDescription $ ucatgr
       subCatgrs <- toValidatedSubCatgrs . usubCatgrRelatedsubCatgrs $ ucatgr
       return ValidatedSubCategory
               { vsubCategoryId = catgrId,
                 vsubCategoryCode = catgrCd,
                 vsubCategoryParentIdCd = parntIdCd,
                 vsubCategoryEnablementStatus = enblmntStatus,
                 vsubCategoryDescription = descpt,
                 vsubCatgrRelatedSubCatgrs = subCatgrs
               }
       where toMaybePrntIdCd :: (String, String) 
                                    -> Either ValidationError (Maybe (CategoryId, CategoryCode))
             toMaybePrntIdCd (prntId, prntCd)
                | null prntId && null prntCd =
                    return Nothing
                | otherwise =
                    do
                    prntCatId <- mapValidationError . crtCatgrId $ prntId
                    prntCatCd <- mapValidationError . crtCatgrCd $ prntCd
                    return . Just $ (prntCatId, prntCatCd)




-- ----------------------------------------------------------------------------
-- Verify reffered sub categories are not either Root or already have a parent step
-- ----------------------------------------------------------------------------




--- TODO: I should probably use a fold here

checkRefSubCatgrsValid :: [Category]
                       -> ValidatedSubCategory
                       -> Either DomainError [CategoryId]
checkRefSubCatgrsValid catgrs =
  traverse (checkRefSubCatgrValid catgrs) . toList . vsubCatgrRelatedSubCatgrs
  where checkRefSubCatgrValid :: [Category] -> CategoryId -> Either DomainError CategoryId
        checkRefSubCatgrValid cats catId =
            let singletonCatgr = filter (\cat -> toCatgrId cat == catId) cats
            in case singletonCatgr of
                    [catgr] -> checkIsSubAndEnabled catId catgr
                    _ -> Left $ DomainError "referenced sub category not found"
            where toCatgrId (RootCategory catgrInfo) = categoryId catgrInfo
                  toCatgrId (SubCategory catgrInfo _) = categoryId catgrInfo



checkRefPrntCatgrEnabled :: Maybe Category -> Either DomainError (Maybe Bool)
checkRefPrntCatgrEnabled (Just (RootCategory CategoryInfo {categoryEnablementStatus = enblmnt})) =
  case enblmnt of
    Disabled reason -> Left . DomainError $ "the parent category is disabled for: " <> reason
    Enabled _ -> return $ Just True
checkRefPrntCatgrEnabled (Just (SubCategory CategoryInfo {categoryEnablementStatus = enblmnt} _)) =
  case enblmnt of
    Disabled reason -> Left . DomainError $ "the parent category is disabled for: " <> reason
    Enabled _ -> return $ Just True
checkRefPrntCatgrEnabled Nothing = return Nothing




-- ----------------------------------------------------------------------------
-- Vefify (if present) that referred parent category **exists** and is not part of
-- the subs categories specified.
-- Its existence will be determined by a database lookup result
-- ----------------------------------------------------------------------------




checkRefPrntCatgrNotInSubs :: Maybe Category 
                           -> ValidatedSubCategory 
                           -> Either DomainError Bool
checkRefPrntCatgrNotInSubs Nothing vSubCatgr =
  return True
checkRefPrntCatgrNotInSubs (Just (RootCategory prntCatgr)) vSubCatgr =
  case filter (parentCatgrIn prntCatgrId) subCatgrIds of
    [] -> return True
    _ -> Left . DomainError $ "parent - sub categories recursion not alowed"
  where
    parentCatgrIn catId subCatId = catId == subCatId
    subCatgrIds = toList . vsubCatgrRelatedSubCatgrs $ vSubCatgr
    prntCatgrId = categoryId prntCatgr
checkRefPrntCatgrNotInSubs (Just (SubCategory prntCatgr _)) vSubCatgr =
  case filter (parentCatgrIn prntCatgrId) subCatgrIds of
    [] -> return True
    _ -> Left . DomainError $ "parent - sub categories recursion not alowed"
  where
    parentCatgrIn catId subCatId = catId == subCatId
    subCatgrIds = toList . vsubCatgrRelatedSubCatgrs $ vSubCatgr
    prntCatgrId = categoryId prntCatgr




-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------




createSubCategory :: ValidatedSubCategory -> Category
createSubCategory vSubCatgr =
  case vsubCategoryParentIdCd vSubCatgr of
    Just (prtCatId, prtCatCode) ->
      SubCategory categoryInfo (Just $ ParentInfo prtCatId prtCatCode)
    Nothing -> SubCategory categoryInfo Nothing
  where
    categoryInfo = CategoryInfo
      { categoryId = vsubCategoryId vSubCatgr,
        categoryCode = vsubCategoryCode vSubCatgr,
        categoryEnablementStatus = vsubCategoryEnablementStatus vSubCatgr,
        categoryDescription = vsubCategoryDescription vSubCatgr,
        categoryRelatedSubCategories = vsubCatgrRelatedSubCatgrs vSubCatgr
      }




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





createSubCatgory :: [Category]
                 -> Maybe Category
                 -> UnvalidatedSubCategory
                 -> UnvalidatedCategoryId
                 -> Either WorkflowError [CreateCategoryEvent]
createSubCatgory referredSubCatgrs
                 referencedParentCatgr
                 unvalidatedCategory 
                 unValidatedCatgrId =
    do validatedCatgr <- mapValidationErr $ validateUnvalidatedCategory unvalidatedCategory
                                                                        unValidatedCatgrId
       _ <- mapDomainErr $ checkRefSubCatgrsValid referredSubCatgrs validatedCatgr
       _ <- mapDomainErr $ checkRefPrntCatgrNotInSubs referencedParentCatgr validatedCatgr
       _ <- mapDomainErr $ checkRefPrntCatgrEnabled referencedParentCatgr
       createdCatgr <- return $ createSubCategory validatedCatgr      
       return $ createEvents createdCatgr
