module CreateRootCategoryPublicTypes where

import CommonSimpleTypes
import CommonCompoundTypes

-- ==========================================================================
-- This file contains the definitions of PUBLIC types 
-- (exposed at the boundary of the bounded context)
-- related to the CreateCategory workflow 
-- ==========================================================================

-- --------------------------------------------------------------------------
-- inputs to the workflow
-- --------------------------------------------------------------------------
type UnvalidatedRootCategoryId = String



data UnvalidatedRootCategory = UnvalidatedRootCategory {
      ucatCd :: String
    , udescpt :: String
    , uEnblmnt :: String
    , usubCatgrs ::   [String]
    } deriving (Eq, Ord, Show)


-- --------------------------------------------------------------------------
-- outputs from the workflow (success case)
-- --------------------------------------------------------------------------


-- Event will be created if the creation is successfull

type RootCategoryCreated = Category
type SubCategoriesAdded = [AddedSubCategory]

data AddedSubCategory = AddedSubCategory {
        parent  :: ParentCategoryId 
    ,   sub     :: SubCategoryId
    } deriving (Eq, Ord, Show)

-- Create Category state (Category type in Common Shared Compound Types)



-- The possible events resulting from the Create Category workflow

data CreateRootCategoryEvent =
        RootCategoryCreated  RootCategoryCreated 
    |   SubCategoriesAdded SubCategoriesAdded
    deriving (Eq, Ord, Show)



-- --------------------------------------------------------------------------
-- error outputs 
-- --------------------------------------------------------------------------



{--
data WorkflowError
      Validation ValidationError 
    | Remote RemoteServiceError
    | DataBase DataBaseError 
    deriving (Eq, Ord, Show)
--}



-- --------------------------------------------------------------------------
-- the workflow itself
-- --------------------------------------------------------------------------



type CreateCategory = 
    UnvalidatedRootCategory -> Either WorkflowError [CreateRootCategoryEvent]

