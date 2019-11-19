module Workflow.CreateAttribute.PublicTypes where

import Common.CompoundTypes
    (AttributeRef)
import Common.SimpleTypes




-- ==========================================================================
-- This file contains the definitions of PUBLIC types
-- (exposed at the boundary of the bounded context)
-- related to the CreateAttributeRef workflow
-- ==========================================================================




-- --------------------------------------------------------------------------
-- inputs to the workflow
-- --------------------------------------------------------------------------



type UnvalidatedAttributeCode = String



data UnvalidatedAttributeRef
  = UnvalidatedAttributeRef
      { uattributeName :: String,
        uattributeDescription :: String,
        uattributesValues :: [String],
        uattributeUnits :: [String],
        urelatedCategories :: [(String, String)]
      }
  deriving (Eq, Ord, Show)




-- --------------------------------------------------------------------------
-- outputs from the workflow (success case)
-- --------------------------------------------------------------------------



-- Event will be created if the creation is successfull
--
--

type AttributeRefCreated = AttributeRef



-- The possible events resulting from the Create Attribute Reference workflow
--
--

newtype CreateAttributeEvent
  = AttributeRefCreated AttributeRefCreated
  deriving (Eq, Ord, Show)



-- --------------------------------------------------------------------------
-- error outputs (See common errors)
-- --------------------------------------------------------------------------



-- --------------------------------------------------------------------------
-- the workflow itself
-- --------------------------------------------------------------------------




type CreateAttribute =
  UnvalidatedAttributeRef -> Either WorkflowError [CreateAttributeEvent]
