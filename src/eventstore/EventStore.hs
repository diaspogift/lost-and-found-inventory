{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.


module EventStore where

import CommonSimpleTypes
import CommonCompoundTypes
import CommonDtos

import CreateRootCategoryDto
import CreateRootCategoryPublicTypes

import CreateSubCategoryDto
import CreateSubCategoryPublicTypes

import CreateAttributeDto
import CreateAttributePublicTypes

import InventorySystemCommands
import DeclaredLostItemPublicTypes

import DeclareLostItemImplementation
import DeclareLostItemDto

import Data.Time
import Data.Maybe

import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.Text.Internal (Text)
import Data.ByteString.Internal (ByteString)



import Data.Text (pack)

import Data.UUID.V4
import Data.UUID hiding (null) -- Internal

import Data.Set hiding (filter, null)

import Control.Monad.Except

import Data.Int


import Data.Either.Combinators

import Control.Applicative


import Control.Concurrent.Async 

import Database.EventStore


import Data.Aeson


--- TODO: refactoring into a shared module ???????????
import CreateAttributeDto
--- ?????????




-- ==========================================================================
-- This file contains the definitions of PUBLIC types 
-- (exposed at the boundary of the bounded context )
-- related to the Declare LostItem workflow 
-- ==========================================================================



-- =============================================================================
-- IO Dependencies types
-- =============================================================================


type LocalStreamId = String


type WriteDeclaredLostItemEvents = 
    Connection -> LocalStreamId -> [DeclareLostItemEvent] -> IO ()

type WriteCreateAttributeRefEvents = 
    Connection -> CreateAttributeEvent -> IO ()

type WriteCreateRootCategoryEvents = 
    Connection -> LocalStreamId -> [CreateRootCategoryEvent] -> IO ()

type WriteCreateSubCategoryEvents = 
    Connection -> LocalStreamId -> [CreateSubCategoryEvent] -> IO ()


type ReadOneCategory  = 
    Connection -> Int32 -> LocalStreamId -> ExceptT WorkflowError IO Category


type ReadOneAttributeRef  = 
    Connection -> Int32 -> LocalStreamId -> ExceptT WorkflowError IO AttributeRef






-- =============================================================================
-- Workflow dependencies dummy Implementations
-- =============================================================================



--- TODO: Consider using a Tree Structure for the  AdministrativeMap data type
---
---








            

-- =============================================================================
-- Event store function impls
-- =============================================================================






---------------------------------------
-- Shared functions
---------------------------------------


readOneCategory :: ReadOneCategory
readOneCategory conn evtNum streamId  =
    do
        rs <- liftIO $ readEventsForward conn (StreamName $ pack $ "root-category- :" <> streamId ) streamStart evtNum NoResolveLink Nothing >>= wait
        case rs of
            ReadSuccess sl@(Slice resolvedEvents mm) -> do
                
                

                let recordedEvts = mapMaybe resolvedEventRecord resolvedEvents
                let pairs = fmap eventDataPair recordedEvts
                let events = fmap eventDataPairTypes pairs
                let reducedEvent = rebuildRootCategoryDto events
                
                liftEither . mapLeft DataBase $ toCategoryDomain reducedEvent

            e -> liftEither . mapLeft DataBase . Left . DataBaseError $ "Read failure: " <> show e
        where 
            eventDataPair recordedEvt = (recordedEventType recordedEvt, recordedEventData recordedEvt)

            eventDataPairTypes :: 
                (Data.Text.Internal.Text, Data.ByteString.Internal.ByteString)
                -> CreateRootCategoryEventDto
            eventDataPairTypes (evtName, strEventData) 
                | evtName == "CreatedRootCategory" = 
                    let rs = fromMaybe  (error "Inconsitant data from event store") (decode . fromStrict $ strEventData :: Maybe RootCategoryCreatedDto)
                    in RootCatCR rs

                | evtName == "SubCategoriesAdded" = 
                    let rs = fromMaybe (error "Inconsitant data from event store") ( decode . fromStrict $ strEventData :: Maybe SubCategoriesAddedDto)
                    in RSubCatsADD rs
                | otherwise = error "invalid event"

            applyDtoEvent :: CreateRootCategoryEventDto -> CreateRootCategoryEventDto -> CreateRootCategoryEventDto
            applyDtoEvent (RootCatCR acc) (RootCatCR elm) = RootCatCR acc
            applyDtoEvent (RootCatCR acc) (RSubCatsADD subs) = 
                let crtSubs = rsubCategrs acc
                    addedSubs = fmap sub subs
                in RootCatCR $ acc { rsubCategrs = crtSubs ++ addedSubs }

            rebuildRootCategoryDto :: [CreateRootCategoryEventDto] -> CreateRootCategoryEventDto
            rebuildRootCategoryDto =  foldr1 applyDtoEvent

            toCategoryDomain :: CreateRootCategoryEventDto -> Either DataBaseError Category
            toCategoryDomain (RootCatCR rtCatgrDto) = 
                let res = rootCatgrDtoToDomain rtCatgrDto
                in case res of
                    Left erroMsg -> Left . DataBaseError $ erroMsg
                    Right result -> return result
    


readOneAttributeRef :: ReadOneAttributeRef
readOneAttributeRef conn evtNum streamId =
    do
        rs <- liftIO $ readEventsForward conn (StreamName $ pack $ "attr-ref-code-: " <> streamId ) streamStart evtNum NoResolveLink Nothing >>= wait
        case rs of
            ReadSuccess sl@(Slice resolvedEvents mm) -> do
                
                

                let recordedEvts = mapMaybe resolvedEventRecord resolvedEvents
                let pairs = fmap eventDataPair1 recordedEvts
                let events = fmap eventDataPairTypes1 pairs
                let reducedEvent = rebuildAttributeRefDtoDto1 events
                
                liftEither . mapLeft DataBase $ toAttributeRefDomain1 reducedEvent

            e -> liftEither . mapLeft DataBase . Left . DataBaseError $ "Read failure: " <> show e
        where
            eventDataPair1 recordedEvt = (recordedEventType recordedEvt, recordedEventData recordedEvt)

            eventDataPairTypes1 :: 
                (Data.Text.Internal.Text, Data.ByteString.Internal.ByteString)
                -> CreateAttributeRefEventDto
            eventDataPairTypes1 (evtName, strEventData) 
                | evtName == "CreatedAttribute" = 
                    let rs = fromMaybe  (error "Inconsitant data from event store") (decode . fromStrict $ strEventData :: Maybe AttributeRefCreatedDto)
                    in CR rs
                | otherwise = error "invalid event"


            applyDtoEvent1 :: CreateAttributeRefEventDto -> CreateAttributeRefEventDto -> CreateAttributeRefEventDto
            applyDtoEvent1 (CR acc) (CR elm) = CR acc


            rebuildAttributeRefDtoDto1 :: [CreateAttributeRefEventDto] -> CreateAttributeRefEventDto
            rebuildAttributeRefDtoDto1 =  foldr1 applyDtoEvent1

            toAttributeRefDomain1 :: CreateAttributeRefEventDto -> Either DataBaseError AttributeRef
            toAttributeRefDomain1 (CR catt) = 
                let res = toDomain1 catt
                in case res of
                    Left erroMsg -> Left . DataBaseError $ erroMsg
                    Right result -> return result
                




---------------------------------------
-- Declare Lost Item
---------------------------------------




writeDeclaredLostItemEvents :: WriteDeclaredLostItemEvents
writeDeclaredLostItemEvents conn streamId evts = 

    do  let persistableEvts = fmap toEvent evts
        as <- sendEvents conn (StreamName $ pack ( "lost-item-stream-id-: " <> streamId)) anyVersion persistableEvts Nothing
        _  <- wait as
        shutdown conn
        waitTillClosed conn
        where toEvent (LostItemDeclared lid) =
                let lidDto = fromLostItemDeclared lid

                    --- TODO this needs some serious clean up
                    --- TODO this needs some serious clean up
                    --- TODO this needs some serious clean up
                    lidDtoN = lidDto {itemAttributes = [], itemLocations = []}
                    --- TODO this needs some serious clean up
                    --- TODO this needs some serious clean up
                    --- TODO this needs some serious clean up
                in createEvent "LostItemDeclared" Nothing $ withJson lidDtoN
              toEvent (LocationsAdded lcsa) =
                let lcsaDto = fromLocationsAdded lcsa
                in createEvent "LocationsAdded" Nothing $ withJson lcsaDto
              toEvent (AttributesAdded attrsa) =
                let attrsaDto = fromAttributesAdded attrsa
                in createEvent "AttributesAdded" Nothing $ withJson attrsaDto


---------------------------------------
-- Create RootCategory
---------------------------------------




writeCreateRootCategoryEvents :: WriteCreateRootCategoryEvents
writeCreateRootCategoryEvents conn streamId evts = 
    do  let persistableEvtss = fmap toEvent evts
        as <- sendEvents conn (StreamName $ pack ( "root-category- :" <> streamId)) anyVersion persistableEvtss Nothing
        _  <- wait as
        shutdown conn
        waitTillClosed conn
        where toEvent (RootCategoryCreated createdRootCat) =
                let createdRootCategoryDto = fromRootCategoryCreated createdRootCat
                in createEvent "CreatedRootCategory" Nothing $ withJson createdRootCategoryDto
              toEvent (RSubCategoriesAdded subCatgrsAdded) =
                let addedSubCatgrDtos = fromRootSubCategoriesAdded subCatgrsAdded
                in createEvent "SubCategoriesAdded" Nothing $ withJson addedSubCatgrDtos






---------------------------------------
-- Create SubCategory
---------------------------------------






writeCreateSubCategoryEvents :: WriteCreateSubCategoryEvents
writeCreateSubCategoryEvents conn streamId evts = 
    do  let persistableEvts = fmap toEvent evts
        as <- sendEvents conn (StreamName $ pack ( "root-category- :" <> streamId)) anyVersion persistableEvts Nothing
        _  <- wait as
        shutdown conn
        waitTillClosed conn
        where toEvent (SubCategoryCreated createdSubCat) =
                let createdSubCategoryDto = fromSubCategoryCreated createdSubCat
                in createEvent "CreatedSubCategory" Nothing $ withJson createdSubCategoryDto
              toEvent (SSubCategoriesAdded subCatgrsAdded) =
                let addedSubCatgrDtos = fromSubSubCategoriesAdded subCatgrsAdded
                in createEvent "SubCategoriesAdded" Nothing $ withJson addedSubCatgrDtos








---------------------------------------
-- Create AttributeRef
---------------------------------------




writeCreateAttributeRefEvents :: WriteCreateAttributeRefEvents
writeCreateAttributeRefEvents conn (AttributeRefCreated createdAttribute) = 
    do  let createdAttributeDto = fromAttributeRefCreated createdAttribute
            createdAttributeEvent = createEvent "CreatedAttribute" Nothing $ withJson createdAttributeDto
            id = code createdAttributeDto
        as <- sendEvent conn (StreamName $ pack ( "attr-ref-code-: " <> id)) anyVersion createdAttributeEvent Nothing
        _  <- wait as
        shutdown conn
        waitTillClosed conn





