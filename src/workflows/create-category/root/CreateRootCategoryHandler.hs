{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.


module CreateRootCategoryHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import InventorySystemCommands
import CreateRootCategoryPublicTypes
import CreateCategoryCommonPublicTypes

import CreateRootCategoryImplementation
import qualified CreateRootCategoryDto as Dto
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

import EventStore

import Data.Char (toUpper)

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
    ReadOneCategory
    -> WriteCreateRootCategoryEvents
    -> NextId
    -> CreateRootCategoryCmd 
    -> ExceptT WorkflowError IO [CreateCategoryEvent]
    
createRootCategoryHandler 
    readOneCategory
    writeCreateRootCategoryEvents
    nextId
    (Command unvalidatedRootCategory curTime userId) = 

        ---------------------------------------- IO at the boundary start -----------------------------------------
     
    do  -- get all referenced sub category / verified they exist and they do not have a parent yet
        refSubCatgrs <- ExceptT $ liftIO $ fmap sequence $ traverse (readOneCategory 10) $ urootCatgrRelatedsubCatgrs unvalidatedRootCategory

        -- get randon uuid for the attribute code 
        unvalidatedCategoryId <- liftIO nextId


        ---------------------------------------- IO at the boundary end -----------------------------------------
    

 
        
    
        ---------------------------------------- Core business logic start ----------------------------------------

        -- call workflow
        let events =
                createRootCatgory 
                    refSubCatgrs
                    unvalidatedRootCategory             -- Input
                    unvalidatedCategoryId                       -- Input
              
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



--- partially applied function for the API (Upper) layer - hinding depencies 
---
---

publicCreateRootCategoryHandler :: CreateRootCategoryCmd -> ExceptT WorkflowError IO [CreateCategoryEvent]
publicCreateRootCategoryHandler = 
    createRootCategoryHandler 
        readOneCategory
        writeCreateRootCategoryEvents
        nextId

        
   
