module CreateAttributeHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import InventorySystemCommands
import CreateAttributePublicTypes

import CreateAttributeImplementation



import Data.UUID.V4
import Data.UUID hiding (null) -- Internal

import Data.Time
import Data.Set hiding (filter, null)
import Control.Monad.Except
import Data.Either.Combinators
import Control.Applicative

import Database.EventStore

import EventStore


import Data.Char (toUpper)











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
    WriteCreateAttributeRefEvents
    -> NextId
    -> CreateAttributeRefCmd 
    -> ExceptT WorkflowError IO [CreateAttributeEvent]
    
createAttributeRefHandler 
    writeCreateAttributeRefEvents
    nextId
    (Command unvalidatedAttributeRef curTime userId) = 

        ---------------------------------------- IO at the boundary start -----------------------------------------
     
    do  -- get all referenced categories / verified they actually exist
        referencedCatgrs <- ExceptT $ liftIO $ fmap sequence $ traverse (readOneCategory 10) $ fst <$> urelatedCategories unvalidatedAttributeRef


        -- get randon uuid for the attribute code 
        attributeRefCode <- liftIO nextId


        ---------------------------------------- IO at the boundary end -----------------------------------------
    
        

    
        ---------------------------------------- Core business logic start ----------------------------------------

        -- call workflow
        let events =
                createAttributeReference 
                    unvalidatedAttributeRef             -- Input
                    attributeRefCode                       -- Input
                    referencedCatgrs                           -- Input
              
        ---------------------------------------- Core business logic end ----------------------------------------




        ---------------------------------------- Side effects handling start ----------------------------------------

        -- publish / persit event(s) into the event store and other interested third parties 
        case events of  
            Right allEvents -> 
                do
                    let crtAttrRefEvet = filter isCreateAttributeRefEvent allEvents
                        evt = head crtAttrRefEvet
                    res <- liftIO $ writeCreateAttributeRefEvents attributeRefCode evt
                    liftEither events
            Left errorMsg -> liftEither $ Left errorMsg

            where isCreateAttributeRefEvent (AttributeRefCreated attrRefCreated) = True

        ---------------------------------------- Side effects handling end ----------------------------------------



--- partially applied function for the API (Upper) layer - hinding depencies 
---
---

publicCreateAttributeRefHandler :: CreateAttributeRefCmd -> ExceptT WorkflowError IO [CreateAttributeEvent]
publicCreateAttributeRefHandler = 
    createAttributeRefHandler 
        writeCreateAttributeRefEvents
        nextId

        
