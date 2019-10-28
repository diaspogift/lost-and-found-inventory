module InventorySystemCommandsHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import DeclaredLostItemPublicTypes
import DeclareLostItemImplementation
import DeclaredLostItemHandler

import CreateAttributePublicTypes
import CreateAttributeImplementation
import CreateAttributeHandler

import CreateRootCategoryPublicTypes
import CreateRootCategoryImplementation
import CreateRootCategoryHandler

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


handle :: LostAndFoundInventoryCmd -> ExceptT InventoryError IO InventoryEvent 
handle command = 

    case command of

        Register declareLostItemCmd ->
            ExceptT . liftIO 
                    . mapBothDeclareLostItemErrorAndEvent 
                    . runExceptT 
                    . publicDeclareLostItemHandler 
                    $ declareLostItemCmd


        CreateAttribute createAttributeRefCmd -> 
            ExceptT . liftIO 
                    . mapBothCreateAttributeErrorAndEvent 
                    . runExceptT 
                    . publicCreateAttributeRefHandler 
                    $ createAttributeRefCmd


        CreateRootCategory createRootCategoryCmd -> 
            ExceptT . liftIO 
                    . mapBothCreateRootCategoryErrorAndEvent 
                    . runExceptT 
                    . publicCreateRootCategoryHandler 
                    $ createRootCategoryCmd    
        
        
        Declare declareFoundItemCmd ->
            ExceptT undefined


        Claim claimFoundItemCmd ->
            ExceptT undefined


        Match catchFoundItemCmd ->
            ExceptT undefined    





--- Helper "mapping" functions
---
---


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

mapBothCreateRootCategoryErrorAndEvent :: IO (Either WorkflowError [CreateRootCategoryEvent]) -> IO (Either InventoryError InventoryEvent)
mapBothCreateRootCategoryErrorAndEvent createCatgrHandler = 
    do  ucreateCatgrHandler <- createCatgrHandler
        case ucreateCatgrHandler of
            Right evts -> return $ Right $ CrteRootCatgrEvt evts 
            Left errMsg -> return $ Left $ CrteRootCatgrErr errMsg



