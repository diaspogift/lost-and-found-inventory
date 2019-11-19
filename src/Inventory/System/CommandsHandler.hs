module Inventory.System.CommandsHandler where

import Common.SimpleTypes (
    WorkflowError
    )
import Control.Monad.Except (
    ExceptT (..), 
    liftIO, 
    runExceptT
    )
import Workflow.CreateAttribute.Handler (
    publicCreateAttributeRefHandler
    )
import Workflow.CreateAttribute.PublicTypes (
    CreateAttributeEvent
    )
import Workflow.CreateCategory.Common.PublicTypes (
    CreateCategoryEvent
    )
import Workflow.CreateCategory.Root.Handler (
    publicCreateRootCategoryHandler
    )
import Workflow.CreateCategory.Sub.Handler (
    publicCreateSubCategoryHandler
    )
import Workflow.DeclareLostItem.Handler (
    publicDeclareLostItemHandler
    )
import Workflow.DeclareLostItem.PublicTypes (
    DeclareLostItemEvent
    )
import Inventory.System.Commands (
    InventoryError (..), 
    InventoryEvent (..), 
    LostAndFoundInventoryCmd (..)
    )



    
-- ==========================================================================================
-- This file contains the implentation details for the command handler of the Inventory
-- bounded context.
-- ==========================================================================================




-- =============================================================================
-- Overall Command Handler Implementation
-- =============================================================================





handle :: LostAndFoundInventoryCmd -> ExceptT InventoryError IO InventoryEvent
handle command =
  case command of
    Register declareLostItemCmd ->
      ExceptT
        . liftIO
        . mapBothDeclareLostItemErrorAndEvent
        . runExceptT
        . publicDeclareLostItemHandler
        $ declareLostItemCmd
    CreateAttribute createAttributeRefCmd ->
      ExceptT
        . liftIO
        . mapBothCreateAttributeErrorAndEvent
        . runExceptT
        . publicCreateAttributeRefHandler
        $ createAttributeRefCmd
    CreateRootCategory createRootCategoryCmd ->
      ExceptT
        . liftIO
        . mapBothCreateRootCategoryErrorAndEvent
        . runExceptT
        . publicCreateRootCategoryHandler
        $ createRootCategoryCmd
    CreateSubCategory createSubCategoryCmd ->
      ExceptT
        . liftIO
        . mapBothCreateSubCategoryErrorAndEvent
        . runExceptT
        . publicCreateSubCategoryHandler
        $ createSubCategoryCmd
    Declare declareFoundItemCmd ->
      ExceptT
        undefined
    Claim claimFoundItemCmd ->
      ExceptT
        undefined
    Match catchFoundItemCmd ->
      ExceptT
        undefined






--- Helper "mapping" functions
---
---


mapBothDeclareLostItemErrorAndEvent ::
  IO (Either WorkflowError [DeclareLostItemEvent]) ->
  IO (Either InventoryError InventoryEvent)
mapBothDeclareLostItemErrorAndEvent resDclLstItmHandler =
  do
    uresDclLstItmHandler <- resDclLstItmHandler
    case uresDclLstItmHandler of
      Right evts -> return $ Right $ DclreLostItemEvt evts
      Left errMsg -> return $ Left $ DclreLostItemErr errMsg




mapBothCreateAttributeErrorAndEvent ::
  IO (Either WorkflowError [CreateAttributeEvent]) ->
  IO (Either InventoryError InventoryEvent)
mapBothCreateAttributeErrorAndEvent createAttrHandler =
  do
    ucreateAttrHandler <- createAttrHandler
    case ucreateAttrHandler of
      Right evts -> return $ Right $ CrteAttribueEvt evts
      Left errMsg -> return $ Left $ CrteAttribueErr errMsg




mapBothCreateRootCategoryErrorAndEvent ::
  IO (Either WorkflowError [CreateCategoryEvent]) ->
  IO (Either InventoryError InventoryEvent)
mapBothCreateRootCategoryErrorAndEvent createCatgrHandler =
  do
    ucreateCatgrHandler <- createCatgrHandler
    case ucreateCatgrHandler of
      Right evts -> return $ Right $ CrteRootCatgrEvt evts
      Left errMsg -> return $ Left $ CrteRootCatgrErr errMsg



      
mapBothCreateSubCategoryErrorAndEvent ::
  IO (Either WorkflowError [CreateCategoryEvent]) ->
  IO (Either InventoryError InventoryEvent)
mapBothCreateSubCategoryErrorAndEvent createCatgrHandler =
  do
    ucreateCatgrHandler <- createCatgrHandler
    case ucreateCatgrHandler of
      Right evts -> return $ Right $ CrteSubCatgrEvt evts
      Left errMsg -> return $ Left $ CrteSubCatgrErr errMsg
