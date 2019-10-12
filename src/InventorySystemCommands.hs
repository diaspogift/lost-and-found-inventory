module InventorySystemCommands where

import CommonSimpleTypes
import CommonCompoundTypes
import DeclaredLostItemPublicTypes
import DeclareLostItemImplementation


import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set hiding (singleton)
import Util
import Data.Either.Combinators

-- ==========================================================================================
-- This file contains the the handlers for all workflows in the Lost |&| Found Inventary 
-- subsystem 
--
--
--
-- ==========================================================================================


-- =============================================================================
-- Types
-- =============================================================================
    

data InventoryCommand d = Command d UTCTime String

type RegisterLostItemCmd = InventoryCommand UnvalidatedLostItem 
type DeclareFoundItemCmd = InventoryCommand UnvalidatedLostItem
type ClaimFoundItemCmd = InventoryCommand UnvalidatedLostItem
type MatchFoundItemCmd = InventoryCommand UnvalidatedLostItem



data LostAndFoundInventoryCmd = 
      Register RegisterLostItemCmd
    | Declare DeclareFoundItemCmd
    | Claim ClaimFoundItemCmd
    | Match MatchFoundItemCmd 



-- =============================================================================
-- Handler Implementation
-- =============================================================================








