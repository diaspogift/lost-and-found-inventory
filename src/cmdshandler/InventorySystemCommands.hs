module InventorySystemCommands where

import CommonSimpleTypes
import CommonCompoundTypes
import DeclaredLostItemPublicTypes
import CreateAttributePublicTypes

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
-- Commands
-- =============================================================================
    

data InventoryCommand d = Command d String String deriving (Eq, Ord, Show)

type DeclareLostItemCmd = InventoryCommand UnvalidatedLostItem 
type DeclareFoundItemCmd = InventoryCommand UnvalidatedLostItem
type ClaimFoundItemCmd = InventoryCommand UnvalidatedLostItem
type MatchFoundItemCmd = InventoryCommand UnvalidatedLostItem
type CreateAttributeRefCmd = InventoryCommand UnvalidatedAttributeRef



data LostAndFoundInventoryCmd = 
      Register DeclareLostItemCmd
    | Declare DeclareFoundItemCmd
    | Claim ClaimFoundItemCmd
    | Match MatchFoundItemCmd 
    | CreateAttribute CreateAttributeRefCmd
    deriving (Eq, Ord, Show)

data InventoryEvent = 
     DclreLostItemEvt [DeclareLostItemEvent]
   | CrteAttribueEvt [CreateAttributeEvent]


data InventoryError = 
   DclreLostItemErr WorkflowError
 | CrteAttribueErr WorkflowError 

   







