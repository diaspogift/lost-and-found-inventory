{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.


module CreateRootCategoryHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import InventorySystemCommands
import CreateRootCategoryPublicTypes

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


type LocalStreamId = String

type WriteEvent = 
    Connection -> LocalStreamId -> [CreateRootCategoryEvent] -> IO ()


type NextId = IO UnvalidatedRootCategoryId





-- =============================================================================
-- Workflow dependencies dummy Implementations
-- =============================================================================

---

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


---


writeEventsToStore :: WriteEvent
writeEventsToStore conn streamId evts = 
    do  let persistableEvts = fmap toEvent evts
        as <- sendEvents conn (StreamName $ pack ( "root-category- :" <> streamId)) anyVersion persistableEvts Nothing
        _  <- wait as
        shutdown conn
        waitTillClosed conn
        where toEvent (RootCategoryCreated createdRootCat) =
                let createdRootCategoryDto = Dto.fromRootCategoryCreated createdRootCat
                in createEvent "CreatedRootCategory" Nothing $ withJson createdRootCategoryDto
              toEvent (SubCategoriesAdded subCatgrsAdded) =
                let addedSubCatgrDtos = Dto.fromSubCategoriesAdded subCatgrsAdded
                in createEvent "SubCategoriesAdded" Nothing $ withJson addedSubCatgrDtos

---




checkRefSubCatgrValidBase :: 
    [(String, Category)] 
    -> UnvalidatedRootCategoryId
    -> Either RefSubCategoryValidationError CategoryId -- CheckRefSubCatgrValid
checkRefSubCatgrValidBase cats ucatId =
    let maybeCat = lookup ucatId cats
    in case maybeCat of
        Just cat ->
            verifyNotRootAndNotSub cat
        Nothing -> 
            Left $ 
                "referenced sub category with id : " 
                ++ ucatId  ++ " not found"
            
        where verifyNotRootAndNotSub Category { enablementStatus = es, rootStatus = rs, categoryId = cid } =
                case es of 
                    Disabled reason ->
                        Left $ 
                            "referenced sub category with id : " 
                            ++ uwrpCatgrId cid  ++ " is disabled for reason: " ++ reason 
                    Enabled ->
                        case rs of
                            Root ->
                                Left $
                                "referenced sub category with id : " 
                                ++ uwrpCatgrId cid ++ " is a root category"
                            Sub maybeParentInfo -> 
                                case maybeParentInfo of 
                                    Just parentInfo -> 
                                        Left $
                                        "referenced sub category with id : " 
                                        ++ uwrpCatgrId cid ++ " is already a sub for: " ++ "<<<<<<<<<< TODO >>>>>>>>>>>>"
                                    Nothing -> Right cid
           
                
checkRefSubCatgrValid :: CheckRefSubCatgrValid
checkRefSubCatgrValid = checkRefSubCatgrValidBase allCategories

-- =============================================================================
-- Create Attribute Ref Command Handler Implementation
-- =============================================================================




createRootCategoryHandler :: 
    LookupOneCategory
    -> WriteEvent
    -> CheckRefSubCatgrValid
    -> NextId
    -> CreateRootCategoryCmd 
    -> ExceptT WorkflowError IO [CreateRootCategoryEvent]
    
createRootCategoryHandler 
    lookupOneCategory
    writeEventToStore
    checkRefSubCatgrValid
    nextId
    (Command unvalidatedRootCategory curTime userId) = 

        ---------------------------------------- IO at the boundary start -----------------------------------------
     
    do  -- get event store connection // TODO: lookup env ... or Reader Monad ??????
        conn <- liftIO $ connect defaultSettings (Static "localhost" 1113)



        -- get all referenced sub category / verified they exist and they do not have a parent yet
        let refSubCatIds = usubCatgrs unvalidatedRootCategory

        refSubCatgrs <- traverse lookupOneCategory refSubCatIds

        -- get randon uuid for the attribute code 
        unvalidatedCategoryId <- liftIO nextId


        ---------------------------------------- IO at the boundary end -----------------------------------------
    


    
        ---------------------------------------- Core business logic start ----------------------------------------

        -- call workflow
        let events =
                createRootCatgory 
                    checkRefSubCatgrValid
                    unvalidatedRootCategory             -- Input
                    unvalidatedCategoryId                       -- Input
              
        ---------------------------------------- Core business logic end ----------------------------------------




        ---------------------------------------- Side effects handling start ----------------------------------------

        -- publish / persit event(s) into the event store and other interested third parties 
        case events of  
            Right allEvents -> 
                do
                    _ <- liftIO $ writeEventsToStore conn unvalidatedCategoryId allEvents

                    liftEither events
            Left errorMsg -> liftEither $ Left errorMsg


        ---------------------------------------- Side effects handling end ----------------------------------------



--- partially applied function for the API (Upper) layer - hinding depencies 
---
---

publicCreateRootCategoryHandler :: CreateRootCategoryCmd -> ExceptT WorkflowError IO [CreateRootCategoryEvent]
publicCreateRootCategoryHandler = 
    createRootCategoryHandler 
        lookupOneCategory
        writeEventsToStore
        checkRefSubCatgrValid
        nextId

        
   
