{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module CreateRootCategoryDto where

import CreateRootCategoryPublicTypes
import Data.Aeson
    (ToJSON,
    FromJSON)
import GHC.Generics
    (Generic)




-- ==========================================================================================
-- This file contains the the logic for working with data transfer objects (DTOs)
--
-- Each type of DTO is defined using primitive, serializable types
--
-- ==========================================================================================




-- ==========================================================================================
-- DTOs for Create Root Category workflow
-- ==========================================================================================




-- ----------------------------------------------------------------------------
-- DTO for CreateAttributeRefForm
-- ----------------------------------------------------------------------------




data CreateRootCategoryForm
  = CreateRootCategoryForm
      { code :: String,
        description :: String,
        enablementStatus :: String,
        relatedSubCategories :: [String]
      }
  deriving (Generic, Show)

instance ToJSON CreateRootCategoryForm
instance FromJSON CreateRootCategoryForm




toUnvalidatedRootCategory :: CreateRootCategoryForm -> UnvalidatedRootCategory
toUnvalidatedRootCategory CreateRootCategoryForm {..} =
  UnvalidatedRootCategory
    { urootCategoryCode = code,
      urootCategoryDescription = description,
      urootCategoryEnablement = enablementStatus,
      urootCatgrRelatedsubCatgrs = relatedSubCategories
    }


    
-------------------------------------------------------------------------------
-- Common DTOs in CreateCategoryCommonDto module
-- ----------------------------------------------------------------------------
