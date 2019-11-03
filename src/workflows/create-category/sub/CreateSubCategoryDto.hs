
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}



module CreateSubCategoryDto where


import Data.Aeson
import CommonSimpleTypes
import CommonDtos
import CommonCompoundTypes
import CreateCategoryCommonPublicTypes
import CreateRootCategoryPublicTypes
import CreateSubCategoryPublicTypes

import CreateCategoryCommonDto



import Prelude hiding (last, id)
import Data.Time
import Data.Set hiding (null, singleton)
import Data.Map hiding (null, toList)

import GHC.Generics







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




data CreateSubCategoryForm = CreateSubCategoryForm {
        code                    :: String
    ,   description             :: String
    ,   parentId                :: String
    ,   parentCode              :: String
    ,   enablementStatus        :: String
    ,   relatedSubCategories    :: [String]
    } deriving (Generic, Show)

instance ToJSON CreateSubCategoryForm 

instance FromJSON CreateSubCategoryForm



-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedSubCategory :: CreateSubCategoryForm -> UnvalidatedSubCategory
toUnvalidatedSubCategory  CreateSubCategoryForm{..}= 
    UnvalidatedSubCategory {
          usubCategoryCode           = code                 
        , usubCategoryDescription    = description          
        , usubCategoryParentIdandCd  = (parentId, parentCode)   
        , usubCatgrEnablementStatus  = enablementStatus 
        , usubCatgrRelatedsubCatgrs  = relatedSubCategories 
        }

        


-------------------------------------------------------------------------------
-- DTO for CategoryCreated Event
-- ----------------------------------------------------------------------------


   