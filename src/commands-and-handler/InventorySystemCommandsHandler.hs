module InventorySystemCommandsHandler where

import CommonSimpleTypes
import CommonCompoundTypes
import DeclaredLostItemPublicTypes
import DeclareLostItemImplementation
import DeclaredLostItemHandler
import InventorySystemCommands

import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set hiding (singleton)
import Util
import Data.Either.Combinators

import Data.UUID


-- ==========================================================================================
--
--
-- This file contains the implentation details for the command handler of the Inventory 
-- bounded context.
--  
--
-- ==========================================================================================



-- =============================================================================
-- Overall Command Handler Implementation
-- =============================================================================


handle :: 
    LostAndFoundInventoryCmd 
    -> EitherIO DeclareLostItemError [DeclareLostItemEvent]
handle cmd = 
    case cmd of
        Register declareLostItemCmd ->
           do res <- publicDeclareLostItemHandler declareLostItemCmd
              return res
        Declare declareFoundItemCmd ->
            return undefined
        Claim claimFoundItemCmd ->
            return undefined
        Match catchFoundItemCmd ->
            return undefined







