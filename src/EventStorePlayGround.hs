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

    let jss = [ object [ "baz" .= True]
              , object [ "foo" .= False]
              , object [ "bar" .= True]
              ]
        evts = fmap (createEvent "foo" Nothing . withJson) jss
    -- _  <- sendEvents conn (StreamName "language") anyVersion evts Nothing >>= wait
    rs <- readEventsForward conn (StreamName "root-category- :f5e7087f-c054-48ca-a733-a494f2a8a4f9") streamStart 10 NoResolveLink Nothing >>= wait
    case rs of
        ReadSuccess sl@(Slice resolvedEvents mm) -> do

            let recordedEvts = mapMaybe resolvedEventRecord resolvedEvents
            let pairs = fmap eventDataPair recordedEvts
            let events = fmap eventDataPairTypes pairs
            let reducedEvent = rebuildRootCategoryDto events
            let domain = toCategoryDomain reducedEvent


            

            print "----------------------pairs--------------------------"
            print "------------------------------------------------"
            print "------------------------------------------------"
            putStrLn ""
            print pairs
            putStrLn ""
            print "------------------------------------------------"
            print "------------------------------------------------"
            print "----------------------pairs--------------------------"


            print "-----------------------events-------------------------"
            print "------------------------------------------------"
            print "------------------------------------------------"
            putStrLn ""
            print events
            putStrLn ""
            print "------------------------------------------------"
            print "------------------------------------------------"
            print "-----------------------events-------------------------"

            print "-----------------------reducedEvent-------------------------"
            print "------------------------------------------------"
            print "------------------------------------------------"
            putStrLn ""
            print reducedEvent
            putStrLn ""
            print "------------------------------------------------"
            print "------------------------------------------------"
            print "-----------------------reducedEvent-------------------------"

            print "-----------------------domain-------------------------"
            print "------------------------------------------------"
            print "------------------------------------------------"
            putStrLn ""
            print domain
            putStrLn ""
            print "------------------------------------------------"
            print "------------------------------------------------"
            print "-----------------------domain-------------------------"

  
            let jss_evts = mapMaybe resolvedEventDataAsJson
                                     $ sliceEvents sl

                                     
 
            assertEqual "Events should be equal" jss jss_evts


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
   



   

