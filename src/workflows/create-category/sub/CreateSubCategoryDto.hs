{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module CreateSubCategoryDto where

import CreateSubCategoryPublicTypes
import Data.Aeson 
    (ToJSON, FromJSON)
import GHC.Generics 
    (Generic)




-- ==========================================================================================
-- This file contains the the logic for working with data transfer objects (DTOs)
-- Each type of DTO is defined using primitive, serializable types
-- ==========================================================================================




-- ==========================================================================================
-- DTOs for Create Root Category workflow
-- ==========================================================================================




-- ----------------------------------------------------------------------------
-- DTO for CreateAttributeRefForm
-- ----------------------------------------------------------------------------




data CreateSubCategoryForm
  = CreateSubCategoryForm
      { code :: String,
        description :: String,
        parentId :: String,
        parentCode :: String,
        enablementStatus :: String,
        relatedSubCategories :: [String]
      }
  deriving (Generic, Show)

instance ToJSON CreateSubCategoryForm
instance FromJSON CreateSubCategoryForm




toUnvalidatedSubCategory :: CreateSubCategoryForm -> UnvalidatedSubCategory
toUnvalidatedSubCategory CreateSubCategoryForm {..} =
  UnvalidatedSubCategory
    { usubCategoryCode = code,
      usubCategoryDescription = description,
      usubCategoryParentIdandCd = (parentId, parentCode),
      usubCatgrEnablementStatus = enablementStatus,
      usubCatgrRelatedsubCatgrs = relatedSubCategories
    }
