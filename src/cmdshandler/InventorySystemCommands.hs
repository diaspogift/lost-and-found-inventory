module InventorySystemCommands where

import CommonSimpleTypes
import CommonCompoundTypes
import DeclaredLostItemPublicTypes
import CreateAttributePublicTypes

import CreateRootCategoryPublicTypes
import CreateCategoryCommonPublicTypes
import CreateSubCategoryPublicTypes

import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set hiding (singleton)
import Util
import Data.Either.Combinators

import Data.UUID


-- ==========================================================================================
-- This file contains the the handlers for all workflows in the Lost |&| Found Inventary 
-- subsystem 
--
--
--
-- ==========================================================================================




-- =============================================================================
-- All possible Commands for the System
-- =============================================================================
    



data InventoryCommand d = Command d String String deriving (Eq, Ord, Show)

type DeclareLostItemCmd = InventoryCommand UnvalidatedLostItem 
type DeclareFoundItemCmd = InventoryCommand UnvalidatedLostItem
type ClaimFoundItemCmd = InventoryCommand UnvalidatedLostItem
type MatchFoundItemCmd = InventoryCommand UnvalidatedLostItem
type CreateAttributeRefCmd = InventoryCommand UnvalidatedAttributeRef
type CreateRootCategoryCmd = InventoryCommand UnvalidatedRootCategory
type CreateSubCategoryCmd = InventoryCommand UnvalidatedSubCategory


data LostAndFoundInventoryCmd = 
      Register DeclareLostItemCmd
    | Declare DeclareFoundItemCmd
    | Claim ClaimFoundItemCmd
    | Match MatchFoundItemCmd 
    | CreateAttribute CreateAttributeRefCmd
    | CreateRootCategory CreateRootCategoryCmd
    | CreateSubCategory CreateSubCategoryCmd
    deriving (Eq, Ord, Show)



    

-- =============================================================================
-- All possibled published Events from workflows
-- =============================================================================
    



data InventoryEvent = 
      DclreLostItemEvt [DeclareLostItemEvent]
    | CrteAttribueEvt [CreateAttributeEvent]
    | CrteRootCatgrEvt [CreateCategoryEvent]
    | CrteSubCatgrEvt [CreateCategoryEvent]




-- =============================================================================
-- All possible Errors from workflows
-- =============================================================================
    



data InventoryError = 
      DclreLostItemErr WorkflowError
    | CrteAttribueErr WorkflowError 
    | CrteRootCatgrErr WorkflowError 
    | CrteSubCatgrErr WorkflowError 

   






