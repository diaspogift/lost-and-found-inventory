{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.
module EventStorePlayGround where

import Control.Concurrent.Async (wait)
import Data.Aeson
import Data.Maybe


import Database.EventStore

import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.Text.Internal (Text)
import Data.ByteString.Internal (ByteString)

import CreateRootCategoryDto
import CreateSubCategoryDto
import CreateAttributeDto
import CommonSimpleTypes
import CommonCompoundTypes
import CommonDtos

import DeclaredLostItemPublicTypes
import DeclareLostItemDto

import CreateCategoryCommonPublicTypes
import CreateRootCategoryPublicTypes
import CreateSubCategoryPublicTypes

import CreateAttributePublicTypes

import Control.Monad.Reader
import Control.Monad.Except

import Data.Either.Combinators
import Data.Int
import Data.Text (pack)







-- =============================================================================
-- Helper types
-- =============================================================================


type LocalStreamId = String

--- Read types
---
---

type ReadOneCategory  = 
    Int32 -> LocalStreamId -> IO (Either WorkflowError Category)


type ReadOneAttributeRef  = 
    Int32 -> LocalStreamId -> IO (Either WorkflowError AttributeRef)

--- Write types
---
---

type WriteDeclaredLostItemEvents = 
    LocalStreamId -> [DeclareLostItemEvent] -> IO ()

type WriteCreateAttributeRefEvents = 
    LocalStreamId -> CreateAttributeEvent -> IO ()

type WriteCreateRootCategoryEvents = 
    LocalStreamId -> [CreateCategoryEvent] -> IO ()

type WriteCreateSubCategoryEvents = 
    LocalStreamId -> [CreateCategoryEvent] -> IO ()





-- =============================================================================
-- Read operations
-- =============================================================================

--- Category


readOneCategoryWithReaderT :: Int32 -> String -> ExceptT  WorkflowError (ReaderT Connection IO) Category
readOneCategoryWithReaderT eventNum streamId = do
    conn <- ask -- gives you the environment which in this case is a String
    rs <- liftIO $ readEventsForward conn (StreamName $ pack $ "root-category- :" <> streamId) streamStart eventNum NoResolveLink Nothing >>= wait
    case rs of
        ReadSuccess sl@(Slice resolvedEvents mm) -> do

            let recordedEvts1 = mapMaybe resolvedEventRecord resolvedEvents
            let pairs = eventDataPair <$> recordedEvts1
            let events = eventDataPairTypes <$> pairs
            let reducedEvent = rebuildRootCategoryDto events
            
            liftEither $ mapLeft DataBase $ toCategoryDomain reducedEvent

        e -> liftEither $ mapLeft DataBase $ Left $ DataBaseError $ "Read Category failure: " <> show e
    where
        
        eventDataPair recordedEvt = (recordedEventType recordedEvt, recordedEventData recordedEvt)

        eventDataPairTypes :: 
            (Data.Text.Internal.Text, Data.ByteString.Internal.ByteString)
            -> CreateCategoryEventDto
        eventDataPairTypes (evtName, strEventData) 
            | evtName == "CreatedCategory" = 
                let rs = fromMaybe  (error "Inconsitant data from event store") (decode . fromStrict $ strEventData :: Maybe CategoryCreatedDto)
                in RootCatCR rs

            | evtName == "SubCategoriesAdded" = 
                let rs = fromMaybe (error "Inconsitant data from event store") ( decode . fromStrict $ strEventData :: Maybe SubCategoriesAddedDto)
                in RSubCatsADD rs

        applyDtoEvent :: CreateCategoryEventDto -> CreateCategoryEventDto -> CreateCategoryEventDto
        applyDtoEvent (RootCatCR acc) (RootCatCR elm) = RootCatCR acc
        applyDtoEvent (RootCatCR acc) (RSubCatsADD subs) = 
            let crtSubs = rsubCategrs acc
                addedSubs = sub <$> subs
            in RootCatCR $ acc { rsubCategrs = crtSubs ++ addedSubs }

        rebuildRootCategoryDto :: [CreateRootCategoryEventDto] -> CreateRootCategoryEventDto
        rebuildRootCategoryDto =  foldr1 applyDtoEvent


        toCategoryDomain :: CreateRootCategoryEventDto -> Either DataBaseError Category
        toCategoryDomain (RootCatCR rtCatgrDto) = 
            let res = rootCatgrDtoToDomain rtCatgrDto
            in case res of
                Left erroMsg -> Left . DataBaseError $ erroMsg
                Right result -> return result







readOneCategory :: Int32 -> String -> IO (Either WorkflowError Category)
readOneCategory num id = do
    conn <- connect defaultSettings (Static "localhost" 1113)
    let uwrpEither = runExceptT $ readOneCategoryWithReaderT num id
    let uwrpReader = runReaderT uwrpEither
    uwrpReader conn





--- AttributeRef


readOneAttributeRefWithReaderT :: Int32 -> String -> ExceptT  WorkflowError (ReaderT Connection IO) AttributeRef
readOneAttributeRefWithReaderT evtNum streamId = do
        conn <- ask
        rs <- liftIO $ readEventsForward conn (StreamName $ pack $ "attr-ref-code-: " <> streamId ) streamStart evtNum NoResolveLink Nothing >>= wait
        case rs of
            ReadSuccess sl@(Slice resolvedEvents mm) -> do
                
                

                let recordedEvts = mapMaybe resolvedEventRecord resolvedEvents
                let pairs = eventDataPair1 <$> recordedEvts
                let events = eventDataPairTypes1 <$> pairs
                let reducedEvent = rebuildAttributeRefDtoDto1 events
                
                liftEither . mapLeft DataBase $ toAttributeRefDomain1 reducedEvent

            e -> liftEither . mapLeft DataBase . Left . DataBaseError $ "Read AttributeRef failure: " <> show e
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


  



readOneAttributeRef :: Int32 -> String -> IO (Either WorkflowError AttributeRef)
readOneAttributeRef num id = do
    conn <- connect defaultSettings (Static "localhost" 1113)
    let uwrpEither = runExceptT $ readOneAttributeRefWithReaderT num id
    let uwrpReader = runReaderT uwrpEither
    uwrpReader conn





-- =============================================================================
-- Write operations
-- =============================================================================





---------------------------------------
-- Declare Lost Item
---------------------------------------




writeDeclaredLostItemEventsWithReaderT :: LocalStreamId -> [DeclareLostItemEvent] -> ReaderT Connection IO ()  
writeDeclaredLostItemEventsWithReaderT streamId evts = 

    do  conn <- ask
        let persistableEvts = toEvent <$> evts
        as <- liftIO $ sendEvents conn (StreamName $ pack ( "lost-item-stream-id-: " <> streamId)) anyVersion persistableEvts Nothing
        _  <- liftIO $ wait as
        liftIO $ shutdown conn
        liftIO $ waitTillClosed conn
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


    
writeDeclaredLostItemEvents :: LocalStreamId -> [DeclareLostItemEvent] -> IO ()
writeDeclaredLostItemEvents id evts = do
    conn <- connect defaultSettings (Static "localhost" 1113)
    let uwrpReader = runReaderT $ writeDeclaredLostItemEventsWithReaderT id evts
    uwrpReader conn



---------------------------------------
-- Create RootCategory
---------------------------------------




writeCreateRootCategoryEventsWithReaderT :: LocalStreamId -> [CreateRootCategoryEvent] -> ReaderT Connection IO ()
writeCreateRootCategoryEventsWithReaderT streamId evts = 
    do  conn <- ask
        let persistableEvtss = toEvent <$> evts
        as <- liftIO $ sendEvents conn (StreamName $ pack ( "root-category- :" <> streamId)) anyVersion persistableEvtss Nothing
        _  <- liftIO $ wait as
        liftIO $ shutdown conn
        liftIO $ waitTillClosed conn
        where toEvent (RootCategoryCreated createdRootCat) =
                let createdRootCategoryDto = fromRootCategoryCreated createdRootCat
                in createEvent "CreatedRootCategory" Nothing $ withJson createdRootCategoryDto
              toEvent (RSubCategoriesAdded subCatgrsAdded) =
                let addedSubCatgrDtos = fromRootSubCategoriesAdded subCatgrsAdded
                in createEvent "SubCategoriesAdded" Nothing $ withJson addedSubCatgrDtos



writeCreateRootCategoryEvents :: LocalStreamId -> [CreateRootCategoryEvent] -> IO ()
writeCreateRootCategoryEvents id evts = do
    conn <- connect defaultSettings (Static "localhost" 1113)
    let uwrpReader = runReaderT $ writeCreateRootCategoryEventsWithReaderT id evts
    uwrpReader conn





---------------------------------------
-- Create SubCategory
---------------------------------------






writeCreateSubCategoryEventsWithReaderT :: LocalStreamId -> [CreateSubCategoryEvent] -> ReaderT Connection IO ()
writeCreateSubCategoryEventsWithReaderT streamId evts = 
    do  conn <- ask
        let persistableEvts = toEvent <$> evts
        as <- liftIO $ sendEvents conn (StreamName $ pack ( "root-category- :" <> streamId)) anyVersion persistableEvts Nothing
        _  <- liftIO $ wait as
        liftIO $ shutdown conn
        liftIO $ waitTillClosed conn
        where toEvent (SubCategoryCreated createdSubCat) =
                let createdSubCategoryDto = fromSubCategoryCreated createdSubCat
                in createEvent "CreatedSubCategory" Nothing $ withJson createdSubCategoryDto
              toEvent (SSubCategoriesAdded subCatgrsAdded) =
                let addedSubCatgrDtos = fromSubSubCategoriesAdded subCatgrsAdded
                in createEvent "SubCategoriesAdded" Nothing $ withJson addedSubCatgrDtos





writeCreateSubCategoryEvents :: LocalStreamId -> [CreateSubCategoryEvent] -> IO ()
writeCreateSubCategoryEvents id evts = do
    conn <- connect defaultSettings (Static "localhost" 1113)
    let uwrpReader = runReaderT $ writeCreateSubCategoryEventsWithReaderT id evts
    uwrpReader conn





---------------------------------------
-- Create AttributeRef
---------------------------------------




writeCreateAttributeRefEventsWithReaderT :: LocalStreamId -> CreateAttributeEvent -> ReaderT Connection IO ()
writeCreateAttributeRefEventsWithReaderT streamId (AttributeRefCreated createdAttribute) = 
    do  conn <- ask
        let createdAttributeDto = fromAttributeRefCreated createdAttribute
            createdAttributeEvent = createEvent "CreatedAttribute" Nothing $ withJson createdAttributeDto
            -- id = code createdAttributeDto
        as <- liftIO $ sendEvent conn (StreamName $ pack ( "attr-ref-code-: " <> streamId)) anyVersion createdAttributeEvent Nothing
        _  <- liftIO $ wait as
        liftIO $ shutdown conn
        liftIO $ waitTillClosed conn


    
writeCreateAttributeRefEvents :: LocalStreamId -> CreateAttributeEvent -> IO ()
writeCreateAttributeRefEvents id evts = do
    conn <- connect defaultSettings (Static "localhost" 1113)
    let uwrpReader = runReaderT $ writeCreateAttributeRefEventsWithReaderT id evts
    uwrpReader conn