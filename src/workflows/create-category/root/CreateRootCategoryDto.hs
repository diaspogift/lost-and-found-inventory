
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}



module CreateRootCategoryDto where


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




data CreateRootCategoryForm = CreateRootCategoryForm {
        code            :: String
    ,   description             :: String
    ,   enablementStatus        :: String
    ,   relatedSubCategories    ::   [String]
    } deriving (Generic, Show, ToJSON, FromJSON)




-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedRootCategory :: CreateRootCategoryForm -> UnvalidatedRootCategory
toUnvalidatedRootCategory  CreateRootCategoryForm{..}= 
    UnvalidatedRootCategory {
            urootCategoryCode           = code                 
        ,   urootCategoryDescription    = description          
        ,   urootCategoryEnablement     = enablementStatus     
        ,   urootCatgrRelatedsubCatgrs  = relatedSubCategories 
        }

        


-------------------------------------------------------------------------------
-- DTO for CategoryCreated Event
-- ----------------------------------------------------------------------------




