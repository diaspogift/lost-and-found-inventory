{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.


module CreateSubCategoryHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import InventorySystemCommands
import CreateSubCategoryPublicTypes

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

type LookupOneMaybeCategory = 
    String -> Maybe Category

type LocalStreamId = String

type WriteEvent = 
    Connection -> LocalStreamId -> [CreateSubCategoryEvent] -> IO ()


type NextId = IO UnvalidatedSubCategoryId





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
            Nothing -> liftEither $ mapLeft DataBase $ Left $ DataBaseError "category not found"


lookupOneMaybeCategoryBase :: 
    [(String, Category)] -> LookupOneMaybeCategory
lookupOneMaybeCategoryBase categories categoryId = 
    lookup categoryId categories
      

lookupOneCategory :: LookupOneCategory 
lookupOneCategory = lookupOneCategoryBase allCategories 


lookupOneMaybeCategory :: LookupOneMaybeCategory 
lookupOneMaybeCategory = lookupOneMaybeCategoryBase allCategories 

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
        where toEvent (SubCategoryCreated createdSubCat) =
                let createdSubCategoryDto = Dto.fromSubCategoryCreated createdSubCat
                in createEvent "CreatedSubCategory" Nothing $ withJson createdSubCategoryDto
              toEvent (SubCategoriesAdded subCatgrsAdded) =
                let addedSubCatgrDtos = Dto.fromSubCategoriesAdded subCatgrsAdded
                in createEvent "SubCategoriesAdded" Nothing $ withJson addedSubCatgrDtos

---




checkRefSubCatgrValidBase :: 
    [(String, Category)] 
    -> UnvalidatedSubCategoryId
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
            
        where verifyNotRootAndNotSub Category { categoryEnablementStatus = es, categoryRootStatus = rs, categoryId = cid } =
                case es of 
                    Disabled reason ->
                        Left $ 
                            "referenced sub category with id : " 
                            ++ uwrpCatgrId cid  ++ " is disabled for reason: " ++ reason 
                    Enabled _ ->
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




createSubCategoryHandler :: 
    LookupOneCategory
    -> LookupOneMaybeCategory
    -> WriteEvent
    -> CheckRefSubCatgrValid
    -> NextId
    -> CreateSubCategoryCmd 
    -> ExceptT WorkflowError IO [CreateSubCategoryEvent]
    
createSubCategoryHandler 
    lookupOneCategory
    lookupOneMaybeCategory
    writeEventToStore
    checkRefSubCatgrValid
    nextId
    (Command unvalidatedSubCategory curTime userId) = 

        ---------------------------------------- IO at the boundary start -----------------------------------------
     
    do  -- get event store connection // TODO: lookup env ... or Reader Monad ??????
        conn <- liftIO $ connect defaultSettings (Static "localhost" 1113)

        -- get all referenced sub category / verified they exist and they do not have a parent yet
        refSubCatgrs <- traverse lookupOneCategory $ usubCatgrs unvalidatedSubCategory

        -- get the eventual referred parent category (fail earlier rather than later :)
        
        let refParentCategory = lookupOneMaybeCategory . fst . uparentIdCd $ unvalidatedSubCategory


        -- get randon uuid for the attribute code 
        unvalidatedCategoryId <- liftIO nextId


        ---------------------------------------- IO at the boundary end -----------------------------------------
    


    
        ---------------------------------------- Core business logic start ----------------------------------------

        -- call workflow
        let events =
                createSubCatgory 
                    refSubCatgrs
                    refParentCategory
                    unvalidatedSubCategory             -- Input
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

publicCreateSubCategoryHandler :: CreateSubCategoryCmd -> ExceptT WorkflowError IO [CreateSubCategoryEvent]
publicCreateSubCategoryHandler = 
    createSubCategoryHandler 
        lookupOneCategory
        lookupOneMaybeCategory
        writeEventsToStore
        checkRefSubCatgrValid
        nextId

        
   
