module InventorySystemCommandsHandler where

import CommonSimpleTypes (
    WorkflowError
    )
import Control.Monad.Except (
    ExceptT (..), 
    liftIO, 
    runExceptT
    )
import CreateAttributeHandler (
    publicCreateAttributeRefHandler
    )
import CreateAttributePublicTypes (
    CreateAttributeEvent
    )
import CreateCategoryCommonPublicTypes (
    CreateCategoryEvent
    )
import CreateRootCategoryHandler (
    publicCreateRootCategoryHandler
    )
import CreateSubCategoryHandler (
    publicCreateSubCategoryHandler
    )
import DeclaredLostItemHandler (
    publicDeclareLostItemHandler
    )
import DeclaredLostItemPublicTypes (
    DeclareLostItemEvent
    )
import InventorySystemCommands (
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

handle ::
  LostAndFoundInventoryCmd ->
  ExceptT InventoryError IO InventoryEvent
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
