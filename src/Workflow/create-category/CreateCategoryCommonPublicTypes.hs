module CreateCategoryCommonPublicTypes where

import Common.CompoundTypes
import Common.SimpleTypes




-- ==========================================================================
-- This file contains the definitions of PUBLIC types
-- (exposed at the boundary of the bounded context)
-- related to the CreateRootCategory workflow
-- ==========================================================================




-- --------------------------------------------------------------------------
-- inputs to the workflow
-- --------------------------------------------------------------------------



type UnvalidatedCategoryId = String




-- --------------------------------------------------------------------------
-- outputs from the workflow (success case)
-- --------------------------------------------------------------------------




--- Event will be created if the creation is successfull
---
---

type CategoryCreated = Category

type SubCategoriesAdded = [AddedSubCategory]




--- Create Category state (Category type in Common Shared Compound Types)
--- data Category = Category {...}
---




--- The possible events resulting from the CreateRootCategory workflow
---
---

data CreateCategoryEvent
  = CategoryCreated CategoryCreated
  | SubCategoriesAdded SubCategoriesAdded
  deriving (Eq, Ord, Show)



-- --------------------------------------------------------------------------
-- error outputs (See common domain errors defined in "common shared types")
-- --------------------------------------------------------------------------
