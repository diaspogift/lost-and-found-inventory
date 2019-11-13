module CreateRootCategoryHandler where

import CommonCompoundTypes
import CommonSimpleTypes
--import CreateAttributeDto

-- Internal

import Control.Monad.Except 
    (ExceptT (..),
    liftEither,
    liftIO)
import CreateCategoryCommonPublicTypes
import CreateRootCategoryImplementation
    (createRootCatgory)
import CreateRootCategoryPublicTypes
import Data.Char 
    (toUpper)
import Data.UUID 
    (toString)
import Data.UUID.V4 
    (nextRandom)
import Database.EventStore
import EventStore 
    (ReadOneCategory, 
    WriteCreateRootCategoryEvents,
    readOneCategory,
    writeCreateRootCategoryEvents
    )
import InventorySystemCommands 
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

---

nextId :: NextId
nextId =
  let id = nextRandom in fmap (fmap toUpper . toString) id

-- =============================================================================
-- Create Attribute Ref Command Handler Implementation
-- =============================================================================

createRootCategoryHandler ::
  ReadOneCategory ->
  WriteCreateRootCategoryEvents ->
  NextId ->
  CreateRootCategoryCmd ->
  ExceptT WorkflowError IO [CreateCategoryEvent]
createRootCategoryHandler
  readOneCategory
  writeCreateRootCategoryEvents
  nextId
  (Command unvalidatedRootCategory curTime userId) =
    ---------------------------------------- IO at the boundary start -----------------------------------------

    do
      -- get all referenced sub category / verified they exist and they do not have a parent yet
      refSubCatgrs 
        <- ExceptT 
            $ liftIO 
            $ fmap sequence 
            $ traverse (readOneCategory 10) 
            $ urootCatgrRelatedsubCatgrs unvalidatedRootCategory
      -- get randon uuid for the attribute code
      unvalidatedCategoryId 
        <- liftIO nextId
      ---------------------------------------- IO at the boundary end -----------------------------------------

      ---------------------------------------- Core business logic start ----------------------------------------

      -- call workflow
      let events =
            createRootCatgory
              refSubCatgrs
              unvalidatedRootCategory -- Input
              unvalidatedCategoryId -- Input

      ---------------------------------------- Core business logic end ----------------------------------------

      ---------------------------------------- Side effects handling start ----------------------------------------

      -- publish / persit event(s) into the event store and other interested third parties
      case events of
        Right allEvents ->
          do
            _ <- liftIO $ writeCreateRootCategoryEvents unvalidatedCategoryId allEvents
            liftEither events
        Left errorMsg -> liftEither $ Left errorMsg

---------------------------------------- Side effects handling end ----------------------------------------

--- partially applied function for the API (Upper) layer - hiding depencies
---
---

publicCreateRootCategoryHandler :: CreateRootCategoryCmd -> ExceptT WorkflowError IO [CreateCategoryEvent]
publicCreateRootCategoryHandler =
  createRootCategoryHandler
    readOneCategory
    writeCreateRootCategoryEvents
    nextId
