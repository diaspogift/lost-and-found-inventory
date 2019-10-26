module InventorySystemCommandsHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import DeclaredLostItemPublicTypes
import DeclareLostItemImplementation
import DeclaredLostItemHandler

import CreateAttributePublicTypes
import CreateAttributeImplementation
import CreateAttributeHandler

import InventorySystemCommands

import Data.Time
import Prelude hiding (last)
import Data.Maybe
import Data.Set hiding (singleton)
import Util
import Data.Either.Combinators

import Data.UUID

import Control.Monad.Except



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
    -> ExceptT InventoryError IO InventoryEvent 
handle cmd = 
    case cmd of
        Register declareLostItemCmd ->
            ExceptT $ liftIO $ mapBothDeclareLostItemErrorAndEvent $ runExceptT $ publicDeclareLostItemHandler declareLostItemCmd
        Declare declareFoundItemCmd ->
            return undefined
        Claim claimFoundItemCmd ->
            return undefined
        Match catchFoundItemCmd ->
            return undefined
        CreateAttribute createAttributeRefCmd -> 
            ExceptT $ liftIO $ mapBothCreateAttributeErrorAndEvent $ runExceptT $ publicCreateAttributeRefHandler createAttributeRefCmd
        



mapBothDeclareLostItemErrorAndEvent :: IO (Either WorkflowError [DeclareLostItemEvent]) -> IO (Either InventoryError InventoryEvent)
mapBothDeclareLostItemErrorAndEvent resDclLstItmHandler = 
    do  uresDclLstItmHandler <- resDclLstItmHandler
        case uresDclLstItmHandler of
            Right evts -> return $ Right $ DclreLostItemEvt evts 
            Left errMsg -> return $ Left $ DclreLostItemErr errMsg

mapBothCreateAttributeErrorAndEvent :: IO (Either WorkflowError [CreateAttributeEvent]) -> IO (Either InventoryError InventoryEvent)
mapBothCreateAttributeErrorAndEvent createAttrHandler = 
    do  ucreateAttrHandler <- createAttrHandler
        case ucreateAttrHandler of
            Right evts -> return $ Right $ CrteAttribueEvt evts 
            Left errMsg -> return $ Left $ CrteAttribueErr errMsg



