module CreateAttributeImplementation where

import CommonCompoundTypes
    (AttributeRef (..),
    Category (..),
    CategoryInfo (..),
    EnablementStatus (..)
    )
import CommonSimpleTypes
import CreateAttributePublicTypes
import Data.Either.Combinators
    (mapLeft)
import Util 
    (singleton)




-- ==========================================================================================
-- This file contains the initial implementation for the createAttrinuteRef workflow
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




type RefCategoryValidationError = String




data ValidatedAttributeRef
  = ValidatedAttributeRef
      { vattrCodeRef :: AttributeCode,
        vattrNameRef :: AttributeName,
        vattrDescriptionRef :: ShortDescription,
        vattrValueRefss :: [AttributeValue],
        vattrUnitRefss :: [AttributeUnit],
        vrelatedCategoriesRefs :: [(CategoryId, CategoryCode)]
      }
  deriving (Eq, Ord, Show)






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




checkRefCatgrEnabled :: Category -> Either DomainError Bool
checkRefCatgrEnabled (RootCategory CategoryInfo {categoryEnablementStatus = enblmnt}) =
  case enblmnt of
    Disabled reason -> Left . DomainError $ "the referred category is disabled for this reason: " <> reason
    Enabled _ -> return True
checkRefCatgrEnabled (SubCategory CategoryInfo {categoryEnablementStatus = enblmnt} _) =
  case enblmnt of
    Disabled reason -> Left . DomainError $ "the referred category is disabled for this reason: " <> reason
    Enabled _ -> return True





-- ----------------------------------------------------------------------------
-- Validation step
-- ----------------------------------------------------------------------------





validateUnvalidatedAttributeRef :: 
    UnvalidatedAttributeRef 
    -> UnvalidatedAttributeCode 
    -> Either ValidationError ValidatedAttributeRef
validateUnvalidatedAttributeRef uattrRef uattrCd =
  ValidatedAttributeRef 
    <$> code 
    <*> name 
    <*> desc 
    <*> values 
    <*> units 
    <*> refCatgrs
  where
    code = toAttributeCodeRef uattrCd
    name = toAttributeName . uattributeName $ uattrRef
    desc = toShortDescpt . uattributeDescription $ uattrRef
    values = traverse toAttributeValue . uattributesValues $ uattrRef
    units = traverse toAttributeUnit . uattributeUnits $ uattrRef
    refCatgrs = traverse toAttributeRefCatgrs . urelatedCategories $ uattrRef





-- ----------------------------------------------------------------------------
-- Creation step
-- ----------------------------------------------------------------------------





createAttrinuteRef :: ValidatedAttributeRef -> AttributeRef
createAttrinuteRef =
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
createEvents =
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
  UnvalidatedAttributeRef ->
  UnvalidatedAttributeCode ->
  [Category] ->
  Either WorkflowError [CreateAttributeEvent]
createAttributeReference
  unvalidatedAttributeRef -- Input
  unValidatedlostItemUuid -- Input
  referencedCategories = -- Input
    do
      -- Validation step

      validatedAttrRef <-
        mapLeft
          Validation
          $ validateUnvalidatedAttributeRef
            unvalidatedAttributeRef
            unValidatedlostItemUuid

      -- Verify that referred categories are all enabled

      _ <-
        mapLeft
          Domain
          $ traverse checkRefCatgrEnabled referencedCategories

      -- Creation step

      createdAttrRef <-
        return $
          createAttrinuteRef
            validatedAttrRef

      -- Events creation step
      
      return $ createEvents createdAttrRef
