module CreateSubCategoryPublicTypes where

import CommonSimpleTypes
import CommonCompoundTypes

-- ==========================================================================
-- This file contains the definitions of PUBLIC types 
-- (exposed at the boundary of the bounded context)
-- related to the CreateSubCategory workflow 
-- ==========================================================================

-- --------------------------------------------------------------------------
-- inputs to the workflow
-- --------------------------------------------------------------------------
type UnvalidatedSubCategoryId = String



data UnvalidatedSubCategory = UnvalidatedSubCategory {
      usubCategoryCode          :: String
    , usubCategoryDescription   :: String
    , usubCategoryParentIdandCd :: (String, String)
    , usubCatgrEnablementStatus :: String
    , usubCatgrRelatedsubCatgrs ::   [String]
    } deriving (Eq, Ord, Show)


-- --------------------------------------------------------------------------
-- outputs from the workflow (success case)
-- --------------------------------------------------------------------------


-- Event will be created if the creation is successfull

type SubCategoryCreated = Category
type SubCategoriesAdded = [AddedSubCategory]

data AddedSubCategory = AddedSubCategory {
        parent  :: ParentCategoryId 
    ,   sub     :: SubCategoryId
    } deriving (Eq, Ord, Show)





-- Create Category state (Category type in Common Shared Compound Types)
-- 
-- data Category = Category {...}
--
--




-- The possible events resulting from the CreateSubCategory workflow

data CreateSubCategoryEvent =
        SubCategoryCreated  SubCategoryCreated 
    |   SubCategoriesAdded SubCategoriesAdded
    deriving (Eq, Ord, Show)



-- --------------------------------------------------------------------------
-- error outputs (Refer to the domain erros defined in The common shared types)
-- --------------------------------------------------------------------------







-- --------------------------------------------------------------------------
-- the workflow itself
-- --------------------------------------------------------------------------



type CreateCategory = 
    UnvalidatedSubCategory -> Either WorkflowError [CreateSubCategoryEvent]

