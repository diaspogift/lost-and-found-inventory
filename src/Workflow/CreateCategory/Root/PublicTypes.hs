module Workflow.CreateCategory.Root.PublicTypes where

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




data UnvalidatedRootCategory
  = UnvalidatedRootCategory
      { urootCategoryCode :: String,
        urootCategoryDescription :: String,
        urootCategoryEnablement :: String,
        urootCatgrRelatedsubCatgrs :: [String]
      }
  deriving (Eq, Ord, Show)




-- --------------------------------------------------------------------------
-- the workflow itself
-- --------------------------------------------------------------------------




type CreateRootCategory =
  UnvalidatedRootCategory -> Either WorkflowError [CreateCategoryEvent]
