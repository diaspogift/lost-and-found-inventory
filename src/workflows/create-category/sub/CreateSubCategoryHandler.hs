module CreateSubCategoryHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import InventorySystemCommands
import CreateSubCategoryPublicTypes
import CreateCategoryCommonPublicTypes

import CreateSubCategoryImplementation
import qualified CreateSubCategoryDto as Dto
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


import Data.Aeson

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

type LookupOneMaybeCategory = 
    String -> Maybe Category


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


--- TODO: Refactor duplicate code

createSubCategoryHandler :: 
    ReadOneCategory
    -> WriteCreateSubCategoryEvents
    -> NextId
    -> CreateSubCategoryCmd 
    -> ExceptT WorkflowError IO [CreateCategoryEvent]
    
createSubCategoryHandler 
    readOneCategory
    writeEventToStore
    nextId
    (Command unvalidatedSubCategory curTime userId) = 

        ---------------------------------------- IO at the boundary start -----------------------------------------
     
    do  -- get all referenced sub category / verified they exist and they do not have a parent yet
        refSubCatgrs <- ExceptT $ liftIO $ fmap sequence $ traverse (readOneCategory 10) $ usubCatgrRelatedsubCatgrs unvalidatedSubCategory

        -- get the eventual referred parent category (fail earlier rather than later :)
        let (strPrntCatId, strPrntCatCd) = usubCategoryParentIdandCd unvalidatedSubCategory

        if notNull strPrntCatId && notNull strPrntCatCd 
        then 
            do  refParentCategory <- ExceptT $ liftIO $ readOneCategory 10 strPrntCatId
                unvalidatedCategoryId <- liftIO nextId

                let events =
                        createSubCatgory 
                            refSubCatgrs
                            (Just refParentCategory)
                            unvalidatedSubCategory             -- Input
                            unvalidatedCategoryId              -- Input

                case events of  
                    Right allEvents -> 
                        do
                            _ <- liftIO $ writeCreateSubCategoryEvents unvalidatedCategoryId allEvents

                            liftEither events
                    Left errorMsg -> liftEither $ Left errorMsg
        else
            do  unvalidatedCategoryId <- liftIO nextId
                let events =
                        createSubCatgory 
                            refSubCatgrs
                            Nothing
                            unvalidatedSubCategory             -- Input
                            unvalidatedCategoryId              -- Input

                case events of  
                    Right allEvents -> 
                        do
                            _ <- liftIO $ writeCreateSubCategoryEvents unvalidatedCategoryId allEvents

                            liftEither events
                    Left errorMsg -> liftEither $ Left errorMsg



    

  


        ---------------------------------------- IO at the boundary end -----------------------------------------
    



    
        ---------------------------------------- Core business logic start ----------------------------------------

        -- call workflow
       
              
        ---------------------------------------- Core business logic end ----------------------------------------




        ---------------------------------------- Side effects handling start ----------------------------------------

        -- publish / persit event(s) into the event store and other interested third parties 
        


        ---------------------------------------- Side effects handling end ----------------------------------------



--- partially applied function for the API (Upper) layer - hinding depencies 
---
---

publicCreateSubCategoryHandler :: CreateSubCategoryCmd -> ExceptT WorkflowError IO [CreateCategoryEvent]
publicCreateSubCategoryHandler = 
    createSubCategoryHandler 
        readOneCategory
        writeCreateSubCategoryEvents
        nextId

        
   
