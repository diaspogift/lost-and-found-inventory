module Workflow.CreateCategory.Root.Handler where

import Common.CompoundTypes
import Common.SimpleTypes
import Control.Monad.Except 
    (ExceptT (..),
    liftEither,
    liftIO)
import Workflow.CreateCategory.Root.PublicTypes
import Workflow.CreateCategory.Root.Dto
import Workflow.CreateCategory.Root.Implementation
    (createRootCatgory)
import Workflow.CreateCategory.Common.PublicTypes
import Data.Char 
    (toUpper)
import Data.UUID 
    (toString)
import Data.UUID.V4 
    (nextRandom)
import Database.EventStore
import Persistence.EventStore 
    (ReadOneCategory, 
    WriteCreateRootCategoryEvents,
    readOneCategory,
    writeCreateRootCategoryEvents
    )
import Inventory.System.Commands 
    (InventoryCommand (..), CreateRootCategoryCmd)




-- ==========================================================================
-- This file contains the definitions of PUBLIC types
-- (exposed at the boundary of the bounded context )
-- related to the Create Attribute Ref workflow
-- ==========================================================================




-- =============================================================================
-- IO Dependencies types
-- =============================================================================




type LookupOneCategory =
  String -> ExceptT WorkflowError IO Category



type NextId = IO UnvalidatedCategoryId




-- =============================================================================
-- Workflow dependencies dummy Implementations
-- =============================================================================




nextId :: NextId
nextId = let randomId = nextRandom in fmap (fmap toUpper . toString) randomId




-- =============================================================================
-- Create Attribute Ref Command Handler Implementation
-- =============================================================================




createRootCategoryHandler :: ReadOneCategory 
                          -> WriteCreateRootCategoryEvents
                          -> NextId
                          -> CreateRootCategoryCmd
                          -> ExceptT WorkflowError IO [CreateCategoryEvent]

createRootCategoryHandler readOneCategory
                          writeCreateRootCategoryEvents
                          nextId
                          (Command unvalidatedRootCategory curTime us_erId) =
  do refSubCatgrs <- ExceptT . liftIO 
                             . fmap sequence 
                             . traverse (readOneCategory 10) 
                             $ urootCatgrRelatedsubCatgrs unvalidatedRootCategory
     unvalidatedCategoryId <- liftIO nextId
     let events = createRootCatgory refSubCatgrs
                                    unvalidatedRootCategory 
                                    unvalidatedCategoryId 
     case events of Right allEvents -> do
                        _ <- liftIO $ writeCreateRootCategoryEvents unvalidatedCategoryId 
                                                                    allEvents
                        liftEither events
                    Left errorMsg -> liftEither $ Left errorMsg





--- partially applied function for the API (Upper) layer - hiding depencies
---
---




publicCreateRootCategoryHandler :: CreateRootCategoryCmd 
                                -> ExceptT WorkflowError IO [CreateCategoryEvent]
publicCreateRootCategoryHandler = createRootCategoryHandler readOneCategory
                                                            writeCreateRootCategoryEvents
                                                            nextId
