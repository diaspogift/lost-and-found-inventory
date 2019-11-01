{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.


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


lookupOneCategoryBase :: 
    [(String, Category)] -> LookupOneCategory
lookupOneCategoryBase categories categoryId = 
    do  let maybeCategory = lookup categoryId categories
        --print maybeCategory
        case maybeCategory of
            Just category -> liftEither $ Right category
            Nothing -> liftEither $ mapLeft DataBase $ Left $ DataBaseError "category not found"


lookupOneCategory :: LookupOneCategory 
lookupOneCategory = lookupOneCategoryBase allCategories 

nextId :: NextId
nextId = 
    let id = nextRandom in fmap toString id



            

-- =============================================================================
-- Create Attribute Ref Command Handler Implementation
-- =============================================================================




createAttributeRefHandler :: 
    LookupOneCategory
    -> WriteCreateAttributeRefEvents
    -> NextId
    -> CreateAttributeRefCmd 
    -> ExceptT WorkflowError IO [CreateAttributeEvent]
    
createAttributeRefHandler 
    lookupOneCategory
    writeCreateAttributeRefEvents
    nextId
    (Command unvalidatedAttributeRef curTime userId) = 

        ---------------------------------------- IO at the boundary start -----------------------------------------
     
    do  -- get event store connection // TODO: lookup env ... or Reader Monad ??????
        conn <- liftIO $ connect defaultSettings (Static "localhost" 1113)


        -- get all referenced categories / verified they actually exist
        refCatgrs <- traverse (readOneCategory conn 10) $ fst <$> urelatedCategories unvalidatedAttributeRef


        -- get randon uuid for the attribute code 
        attributeCode <- liftIO nextId


        ---------------------------------------- IO at the boundary end -----------------------------------------
    
        

    
        ---------------------------------------- Core business logic start ----------------------------------------

        -- call workflow
        let events =
                createAttributeReference 
                    unvalidatedAttributeRef             -- Input
                    attributeCode                       -- Input
              
        ---------------------------------------- Core business logic end ----------------------------------------




        ---------------------------------------- Side effects handling start ----------------------------------------

        -- publish / persit event(s) into the event store and other interested third parties 
        case events of  
            Right allEvents -> 
                do
                    let crtAttrRefEvet = filter isCreateAttributeRefEvent allEvents
                        evt = head crtAttrRefEvet
                    res <- liftIO $ writeCreateAttributeRefEvents conn evt
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
        lookupOneCategory
        writeCreateAttributeRefEvents
        nextId

        
