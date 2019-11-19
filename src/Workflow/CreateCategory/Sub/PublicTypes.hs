module Workflow.CreateCategory.Sub.PublicTypes where

import Common.CompoundTypes
import Common.SimpleTypes
import Workflow.CreateCategory.Common.PublicTypes




-- ==========================================================================
-- This file contains the definitions of PUBLIC types
-- (exposed at the boundary of the bounded context)
-- related to the CreateRootCategory workflow
-- ==========================================================================




-- --------------------------------------------------------------------------
-- inputs to the workflow
-- --------------------------------------------------------------------------




data UnvalidatedSubCategory
  = UnvalidatedSubCategory
      { usubCategoryCode :: String,
        usubCategoryDescription :: String,
        usubCategoryParentIdandCd :: (String, String),
        usubCatgrEnablementStatus :: String,
        usubCatgrRelatedsubCatgrs :: [String]
      }
  deriving (Eq, Ord, Show)




-- --------------------------------------------------------------------------
-- the workflow itself
-- --------------------------------------------------------------------------




type CreateSubCategory =
  UnvalidatedSubCategory -> Either WorkflowError [CreateCategoryEvent]
