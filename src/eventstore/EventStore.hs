{-# LANGUAGE OverloadedStrings #-}

-- That library uses `Text` pervasively. This pragma permits to use
-- String literal when a Text is needed.
module EventStore where

import CommonCompoundTypes (
        Category,
        AttributeRef
        )
import CommonDtos (
        sub
        )
import CommonSimpleTypes (
        DataBaseError (..),
        LostItemId,
        WorkflowError (..),
        mapDataBase,

        )
import Control.Concurrent.Async (
    wait
    )
import Control.Monad.Except (
        ExceptT (..),
        liftEither,
        runExceptT
        )
import Control.Monad.Reader (
        ReaderT (..),
        liftIO,
        ask
        )
import CreateAttributeDto (
    AttributeRefCreatedDto, 
    CreateAttributeRefEventDto (..),
    toAttributeRefDomain,
    fromAttributeRefCreated
    )
import CreateAttributePublicTypes (
    CreateAttributeEvent (..)
    )
import CreateCategoryCommonDto (
    CreateCategoryEventDto (..),
    CategoryCreatedDto,
    SubCategoriesAddedDto,
    subCategrs,
    catgrDtoToDomain,
    fromCategoryCreated,
    fromSubCategoriesAdded
    )
import CreateCategoryCommonPublicTypes (
    CategoryCreated,
    SubCategoriesAdded,
    CreateCategoryEvent (..)
    )
import Data.Aeson (
    decode
    )
import Data.ByteString.Internal (
    ByteString
    )
import Data.ByteString.Lazy.Char8 (
    fromStrict
    )
import Data.Either.Combinators (
    mapLeft
    )
import Data.Int (
    Int32
    )
import Data.Maybe (
    fromMaybe,
    mapMaybe
    )
import Data.Text (
    pack
    )
import Data.Text.Internal (
    Text
    )
import Database.EventStore
import DeclareLostItemDto (
    itemAttributes,
    itemLocations,
    fromLostItemDeclared,
    fromLocationsAdded,
    fromAttributesAdded
    )
import DeclareLostItemPublicTypes (
    DeclaredLostItem,
    DeclareLostItemEvent (..)
    )




-- =============================================================================
-- Helper types
-- =============================================================================




type LocalStreamId = String




--- Read types
---
---




type ReadOneCategory =
  Int32 -> LocalStreamId -> IO (Either WorkflowError Category)

type ReadOneDeclaredLostItem =
  Int32 -> LostItemId -> IO (Either WorkflowError DeclaredLostItem)

type ReadOneAttributeRef =
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




---------------------------------------
-- Declare Lost Item
---------------------------------------




readOneDeclaredLostItem :: ReadOneDeclaredLostItem
readOneDeclaredLostItem = undefined




---------------------------------------
--- Category
---------------------------------------




readOneCategoryWithReaderT :: Int32 -> String -> ExceptT WorkflowError (ReaderT Connection IO) Category
readOneCategoryWithReaderT eventNum streamId = do
  conn <- ask -- gives you the environment which in this case is a String
  rs <- liftIO $ readEventsForward conn (StreamName $ pack streamId) streamStart eventNum NoResolveLink Nothing >>= wait
  case rs of
    ReadSuccess sl@(Slice resolvedEvents mm) -> do
      let recordedEvts1 = mapMaybe resolvedEventRecord resolvedEvents
      let pairs = eventDataPair <$> recordedEvts1
      let events = eventDataPairTypes <$> pairs
      let reducedEvent = rebuildCategoryDto events
      liftEither . mapDataBase . toCategoryDomain $ reducedEvent
    e -> liftEither . mapDataBase . Left . DataBaseError $ "Read Category failure: " <> show e
  where
    eventDataPair recordedEvt = (recordedEventType recordedEvt, recordedEventData recordedEvt)
    eventDataPairTypes ::
      (Data.Text.Internal.Text, Data.ByteString.Internal.ByteString) ->
      CreateCategoryEventDto
    eventDataPairTypes (evtName, strEventData)
      | evtName == "CreatedCategory" =
        let rs = fromMaybe (error "Inconsitant data from event store") (decode . fromStrict $ strEventData :: Maybe CategoryCreatedDto)
         in CatgrCreated rs
      | evtName == "SubCategoriesAdded" =
        let rs = fromMaybe (error "Inconsitant data from event store") (decode . fromStrict $ strEventData :: Maybe SubCategoriesAddedDto)
         in SubCatgrsAdded rs


    applyDtoEvent :: CreateCategoryEventDto -> CreateCategoryEventDto -> CreateCategoryEventDto
    applyDtoEvent (CatgrCreated acc) (CatgrCreated elm) = CatgrCreated acc
    applyDtoEvent (CatgrCreated acc) (SubCatgrsAdded subs) =
      let crtSubs = subCategrs acc
          addedSubs = sub <$> subs
       in CatgrCreated $ acc {subCategrs = crtSubs ++ addedSubs}
    rebuildCategoryDto :: [CreateCategoryEventDto] -> CreateCategoryEventDto
    rebuildCategoryDto = foldr1 applyDtoEvent
    toCategoryDomain :: CreateCategoryEventDto -> Either DataBaseError Category
    toCategoryDomain (CatgrCreated catgrDto) =
      let res = catgrDtoToDomain catgrDto
       in case res of
            Left erroMsg -> Left . DataBaseError $ erroMsg
            Right result -> return result






readOneCategory :: Int32 -> String -> IO (Either WorkflowError Category)
readOneCategory num id = do
  conn <- connect defaultSettings (Static "localhost" 1113)
  let uwrpEither = runExceptT $ readOneCategoryWithReaderT num id
  let uwrpReader = runReaderT uwrpEither
  uwrpReader conn





---------------------------------------
--- AttributeRef
---------------------------------------





readOneAttributeRefWithReaderT :: Int32 -> String -> ExceptT WorkflowError (ReaderT Connection IO) AttributeRef
readOneAttributeRefWithReaderT evtNum streamId = do
  conn <- ask
  rs <- liftIO $ readEventsForward conn (StreamName $ pack streamId) streamStart evtNum NoResolveLink Nothing >>= wait
  case rs of
    ReadSuccess (Slice resolvedEvents _) -> do
      let recordedEvts = mapMaybe resolvedEventRecord resolvedEvents
      let pairs = eventDataPair1 <$> recordedEvts
      let events = eventDataPairTypes1 <$> pairs
      let reducedEvent = rebuildAttributeRefDtoDto1 events
      liftEither . mapDataBase . toAttributeRefDomain1 $ reducedEvent
    e -> liftEither . mapDataBase . Left . DataBaseError $ "Read AttributeRef failure: " <> show e
  where
    eventDataPair1 recordedEvt = (recordedEventType recordedEvt, recordedEventData recordedEvt)
    eventDataPairTypes1 ::
      (Data.Text.Internal.Text, Data.ByteString.Internal.ByteString) ->
      CreateAttributeRefEventDto
    eventDataPairTypes1 (evtName, strEventData)
      | evtName == "CreatedAttribute" =
        let rs = fromMaybe (error "Inconsitant data from event store") (decode . fromStrict $ strEventData :: Maybe AttributeRefCreatedDto)
         in CR rs
      | otherwise = error "invalid event"
    applyDtoEvent1 :: CreateAttributeRefEventDto -> CreateAttributeRefEventDto -> CreateAttributeRefEventDto
    applyDtoEvent1 (CR acc) (CR elm) = CR acc
    rebuildAttributeRefDtoDto1 :: [CreateAttributeRefEventDto] -> CreateAttributeRefEventDto
    rebuildAttributeRefDtoDto1 = foldr1 applyDtoEvent1
    toAttributeRefDomain1 :: CreateAttributeRefEventDto -> Either DataBaseError AttributeRef
    toAttributeRefDomain1 (CR catt) =
      let res = toAttributeRefDomain catt
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
  do
    connec <- ask
    let persistableEvts = toEvent <$> evts
    as <- liftIO $ sendEvents connec (StreamName $ pack streamId) anyVersion persistableEvts Nothing
    _ <- liftIO $ wait as
    liftIO $ shutdown connec
    liftIO $ waitTillClosed connec
  where
    toEvent (LostItemDeclared lid) =
      let lidDto = fromLostItemDeclared lid
          --- TODO this needs some serious clean up
          --- TODO this needs some serious clean up
          --- TODO this needs some serious clean up
          lidDtoN = lidDto {itemAttributes = [], itemLocations = []}
       in --- TODO this needs some serious clean up
          --- TODO this needs some serious clean up
          --- TODO this needs some serious clean up
          createEvent "LostItemDeclared" Nothing $ withJson lidDtoN
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





writeCreateRootCategoryEventsWithReaderT :: LocalStreamId -> [CreateCategoryEvent] -> ReaderT Connection IO ()
writeCreateRootCategoryEventsWithReaderT streamId evts =
  do
    conn <- ask
    let persistableEvtss = toEvent <$> evts
    as <- liftIO $ sendEvents conn (StreamName $ pack streamId) anyVersion persistableEvtss Nothing
    _ <- liftIO $ wait as
    liftIO $ shutdown conn
    liftIO $ waitTillClosed conn
  where
    toEvent (CategoryCreated createdCatgr) =
      let createdCategoryDto = fromCategoryCreated createdCatgr
       in createEvent "CreatedCategory" Nothing $ withJson createdCategoryDto
    toEvent (SubCategoriesAdded subCatgrsAdded) =
      let addedSubCatgrDtos = fromSubCategoriesAdded subCatgrsAdded
       in createEvent "SubCategoriesAdded" Nothing $ withJson addedSubCatgrDtos

writeCreateRootCategoryEvents :: LocalStreamId -> [CreateCategoryEvent] -> IO ()
writeCreateRootCategoryEvents id evts = do
  conn <- connect defaultSettings (Static "localhost" 1113)
  let uwrpReader = runReaderT $ writeCreateRootCategoryEventsWithReaderT id evts
  uwrpReader conn





---------------------------------------
-- Create SubCategory
---------------------------------------





writeCreateSubCategoryEventsWithReaderT :: LocalStreamId -> [CreateCategoryEvent] -> ReaderT Connection IO ()
writeCreateSubCategoryEventsWithReaderT streamId evts =
  do
    conn <- ask
    let persistableEvts = toEvent <$> evts
    as <- liftIO $ sendEvents conn (StreamName $ pack streamId) anyVersion persistableEvts Nothing
    _ <- liftIO $ wait as
    liftIO $ shutdown conn
    liftIO $ waitTillClosed conn
  where
    toEvent (CategoryCreated createdSubCat) =
      let createdCategoryDto = fromCategoryCreated createdSubCat
       in createEvent "CreatedCategory" Nothing $ withJson createdCategoryDto
    toEvent (SubCategoriesAdded subCatgrsAdded) =
      let addedSubCatgrDtos = fromSubCategoriesAdded subCatgrsAdded
       in createEvent "SubCategoriesAdded" Nothing $ withJson addedSubCatgrDtos

writeCreateSubCategoryEvents :: LocalStreamId -> [CreateCategoryEvent] -> IO ()
writeCreateSubCategoryEvents id evts = do
  conn <- connect defaultSettings (Static "localhost" 1113)
  let uwrpReader = runReaderT $ writeCreateSubCategoryEventsWithReaderT id evts
  uwrpReader conn





---------------------------------------
-- Create AttributeRef
---------------------------------------





writeCreateAttributeRefEventsWithReaderT :: LocalStreamId -> CreateAttributeEvent -> ReaderT Connection IO ()
writeCreateAttributeRefEventsWithReaderT streamId (AttributeRefCreated createdAttribute) =
  do
    conn <- ask
    let createdAttributeDto = fromAttributeRefCreated createdAttribute
        createdAttributeEvent = createEvent "CreatedAttribute" Nothing $ withJson createdAttributeDto
    -- id = code createdAttributeDto
    as <- liftIO $ sendEvent conn (StreamName $ pack streamId) anyVersion createdAttributeEvent Nothing
    _ <- liftIO $ wait as
    liftIO $ shutdown conn
    liftIO $ waitTillClosed conn

writeCreateAttributeRefEvents :: LocalStreamId -> CreateAttributeEvent -> IO ()
writeCreateAttributeRefEvents id evts = do
  conn <- connect defaultSettings (Static "localhost" 1113)
  let uwrpReader = runReaderT $ writeCreateAttributeRefEventsWithReaderT id evts
  uwrpReader conn
