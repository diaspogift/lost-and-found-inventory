{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.


module CreateAttributeHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import InventorySystemCommands
import CreateAttributePublicTypes

import CreateAttributeImplementation
import qualified CreateAttributeDto as Dto
--import CreateAttributeDto

import Data.Time

import Data.Text (pack)

import Data.UUID.V4
import Data.UUID hiding (null) -- Internal

import Data.Set hiding (filter, null)

import Control.Monad.Except


import Data.Either.Combinators

import Control.Applicative


import Control.Concurrent.Async 

import Database.EventStore


import Data.Aeson




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


type WriteEvent = 
    Connection -> CreateAttributeEvent -> IO ()


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
            Nothing -> liftEither $ mapLeft Db $ Left $ DbError "category not found"


lookupOneCategory :: LookupOneCategory 
lookupOneCategory = lookupOneCategoryBase allCategories 

nextId :: NextId
nextId = 
    let id = nextRandom in fmap toString id



writeEventToStore :: WriteEvent
writeEventToStore conn (AttributeRefCreated createdAttribute) = 
    do  let createdAttributeDto = Dto.fromAttributeRefCreated createdAttribute
            createdAttributeEvent = createEvent "CreatedAttribute" Nothing $ withJson createdAttributeDto
            id = Dto.attrCode createdAttributeDto
        as <- sendEvent conn (StreamName $ pack ( "attr-ref-code-: " <> id)) anyVersion createdAttributeEvent Nothing
        _  <- wait as
        shutdown conn
        waitTillClosed conn


            

-- =============================================================================
-- Create Attribute Ref Command Handler Implementation
-- =============================================================================




createAttributeRefHandler :: 
    LookupOneCategory
    -> WriteEvent
    -> NextId
    -> CreateAttributeRefCmd 
    -> ExceptT WorkflowError IO [CreateAttributeEvent]
    
createAttributeRefHandler 
    lookupOneCategory
    writeEventToStore
    nextId
    (Command unvalidatedAttributeRef curTime userId) = 

        ---------------------------------------- IO at the boundary start -----------------------------------------
     
    do  -- get event store connection // TODO: lookup env ... or Reader Monad ??????
        conn <- liftIO $ connect defaultSettings (Static "localhost" 1113)



        -- get all referenced category / verified they exist
        let refCatIds = fst <$> urelatedCatgrs unvalidatedAttributeRef

        refCatgrs <- traverse lookupOneCategory refCatIds

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
                    res <- liftIO $ writeEventToStore conn evt
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
        writeEventToStore
        nextId

        
   
