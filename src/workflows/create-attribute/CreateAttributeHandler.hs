module CreateAttributeHandler where

import CommonCompoundTypes
    (Category)
import CommonSimpleTypes
import Control.Monad.Except 
    (ExceptT (..),
    liftEither,
    liftIO
    )
import CreateAttributeImplementation 
    (createAttributeReference)
import CreateAttributePublicTypes
import Data.Char 
    (toUpper)
import Data.UUID 
    (toString)
import Data.UUID.V4 
    (nextRandom)
import Database.EventStore
import EventStore 
    (WriteCreateAttributeRefEvents,
    readOneCategory,
    writeCreateAttributeRefEvents)
import InventorySystemCommands 
    ( InventoryCommand (..),
    CreateAttributeRefCmd
    )

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

type NextId = IO UnvalidatedAttributeCode



-- =============================================================================
-- Workflow dependencies dummy Implementations
-- =============================================================================



nextId :: NextId
nextId =
  let id = nextRandom in fmap (fmap toUpper . toString) id



-- =============================================================================
-- Create Attribute Ref Command Handler Implementation
-- =============================================================================



createAttributeRefHandler ::
  WriteCreateAttributeRefEvents ->
  NextId ->
  CreateAttributeRefCmd ->
  ExceptT WorkflowError IO [CreateAttributeEvent]
createAttributeRefHandler
  writeCreateAttributeRefEvents
  nextId
  (Command unvalidatedAttributeRef curTime userId) =
    ---------------------------------------- IO at the boundary start -----------------------------------------

    do
      -- get all referenced categories / verified they actually exist
      referencedCatgrs 
        <- ExceptT 
            $ liftIO 
            $ fmap sequence 
            $ traverse (readOneCategory 10) 
            $ fst <$> urelatedCategories unvalidatedAttributeRef
      -- get randon uuid for the attribute code
      attributeRefCode 
        <- liftIO nextId
      ---------------------------------------- IO at the boundary end -----------------------------------------

      ---------------------------------------- Core business logic start --------------------------------------

      -- call workflow
      let events =
            createAttributeReference
              unvalidatedAttributeRef -- Input
              attributeRefCode -- Input
              referencedCatgrs -- Input

      ---------------------------------------- Core business logic end ----------------------------------------

      ---------------------------------------- Side effects handling start ------------------------------------

      -- publish / persit event(s) into the event store and other interested third parties
      case events of
        Right allEvents ->
          do
            let crtAttrRefEvet = filter isCreateAttributeRefEvent allEvents
                evt = head crtAttrRefEvet
            res <- liftIO $ writeCreateAttributeRefEvents attributeRefCode evt
            liftEither events
        Left errorMsg -> liftEither $ Left errorMsg
    where
      isCreateAttributeRefEvent (AttributeRefCreated attrRefCreated) = True

    ---------------------------------------- Side effects handling end ----------------------------------------




--- partially applied function for the API (Upper) layer - hiding depencies
---
---

publicCreateAttributeRefHandler :: 
    CreateAttributeRefCmd 
    -> ExceptT WorkflowError IO [CreateAttributeEvent]
publicCreateAttributeRefHandler =
  createAttributeRefHandler
    writeCreateAttributeRefEvents
    nextId
