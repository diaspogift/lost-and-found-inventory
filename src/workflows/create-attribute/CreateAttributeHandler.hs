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
    (InventoryCommand (..),
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
nextId = let randomId = nextRandom in fmap (fmap toUpper . toString) randomId




-- =============================================================================
-- Create Attribute Ref Command Handler Implementation
-- =============================================================================





createAttributeRefHandler :: WriteCreateAttributeRefEvents
                                -> NextId
                                -> CreateAttributeRefCmd
                                -> ExceptT WorkflowError IO [CreateAttributeEvent]
createAttributeRefHandler   writeCreateAttributeRefEvents
                            nextId
                            (Command unvalidatedAttributeRef curTime userId) =
    do referencedCatgrs <- ExceptT . liftIO 
                                   . fmap sequence 
                                   . traverse (readOneCategory 10) 
                                   $ fst <$> urelatedCategories unvalidatedAttributeRef
       attributeRefCode <- liftIO nextId
       let events = createAttributeReference unvalidatedAttributeRef 
                                             attributeRefCode 
                                             referencedCatgrs 
       case events of Right allEvents -> do
                        let crtAttrRefEvt = filter isCreateAttributeRefEvent allEvents
                            evt = head crtAttrRefEvt
                        res <- liftIO $ writeCreateAttributeRefEvents attributeRefCode 
                                                                      evt
                        liftEither events
                      Left errorMsg -> liftEither $ Left errorMsg
                      
    where isCreateAttributeRefEvent (AttributeRefCreated attrRefCreated) = True





--- partially applied function for the API (Upper) layer - hiding depencies
---
---



publicCreateAttributeRefHandler :: CreateAttributeRefCmd 
                                    -> ExceptT WorkflowError IO [CreateAttributeEvent]
publicCreateAttributeRefHandler = createAttributeRefHandler writeCreateAttributeRefEvents
                                                            nextId
