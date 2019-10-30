{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.
module EventStorePlayGround where

import Control.Concurrent.Async (wait)
import Data.Aeson
import Data.Maybe

import Test.Tasty.HUnit
import Test.Tasty.Hspec

import Database.EventStore

import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.Text.Internal (Text)
import Data.ByteString.Internal (ByteString)

import CreateRootCategoryDto
import CommonSimpleTypes
import CommonCompoundTypes


eventStore :: IO ()
eventStore = do

    conn <- connect defaultSettings (Static "localhost" 1113)

    rs <- readEventsForward conn (StreamName "root-category- :fd14dd03-983a-4596-95a8-ff27b04b0523") streamStart 10 NoResolveLink Nothing >>= wait
    case rs of
        ReadSuccess sl@(Slice resolvedEvents mm) -> do



            
            putStrLn "----------------------sl--------------------------"
            print sl
            putStrLn "----------------------sl--------------------------"


            let recordedEvts = mapMaybe resolvedEventRecord resolvedEvents
            let pairs = fmap eventDataPair recordedEvts
            let events = fmap eventDataPairTypes pairs
            let reducedEvent = rebuildRootCategoryDto events
            let domain = toCategoryDomain reducedEvent



            putStrLn "----------------------pairs--------------------------"
            print pairs
            putStrLn "----------------------pairs--------------------------"


            putStrLn "----------------------events--------------------------"
            print events
            putStrLn ""
            putStrLn "----------------------events--------------------------"


            putStrLn "-----------------------reducedEvent-------------------------"
            putStrLn ""
            print reducedEvent
            putStrLn "-----------------------reducedEvent-------------------------"


            putStrLn "-----------------------domain-------------------------"
            print domain
            putStrLn "-----------------------domain-------------------------"


        e -> fail $ "Read failure: " <> show e


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
        in SubCatsADD rs
 

applyDtoEvent :: CreateRootCategoryEventDto -> CreateRootCategoryEventDto -> CreateRootCategoryEventDto
applyDtoEvent (RootCatCR acc) (RootCatCR elm) = RootCatCR acc
applyDtoEvent (RootCatCR acc) (SubCatsADD subs) = 
    let crtSubs = subCategrs acc
        addedSubs = fmap sub subs
    in RootCatCR $ acc { subCategrs = crtSubs ++ addedSubs }

rebuildRootCategoryDto :: [CreateRootCategoryEventDto] -> CreateRootCategoryEventDto
rebuildRootCategoryDto =  foldr1 applyDtoEvent

toCategoryDomain :: CreateRootCategoryEventDto -> Either ErrorMessage Category
toCategoryDomain (RootCatCR rtCatgrDto) = toDomain rtCatgrDto
   



   

