{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.
module EventStorePlayGround where

import Control.Concurrent.Async (wait)
import Data.Aeson
-- It requires to have `aeson` package installed. Note that EventStore doesn't constraint you to JSON
-- format but putting common use aside, by doing so you'll be able to use some interesting EventStore
-- features like its Complex Event Processing (CEP) capabality.

import Database.EventStore
-- Note that imports 'NonEmpty' data constructor and 'nonEmpty' function from
-- 'Data.List.NonEmpty'.

eventStore :: IO ()
eventStore = do
   
    conn <- connect defaultSettings (Static "localhost" 1113)
    let js  = object ["isHaskellTheBest" .= True] -- (.=) comes from Data.Aeson module.
        evt = createEvent "programming" Nothing (withJson js)

    -- Appends an event to a stream named `languages`.
    as <- sendEvent conn (StreamName "languagesnew") anyVersion evt Nothing

    -- EventStore interactions are fundamentally asynchronous. Nothing requires you to wait
    -- for the completion of an operation, but it's good to know if something went wrong.
    _ <- wait as

    -- Again, if you decide to `shutdown` an EventStore connection, it means your application is
    -- about to terminate.
    shutdown conn

    -- Make sure the EventStore connection completes every ongoing operation. For instance, if
    -- at the moment we call `shutdown` and some operations (or subscriptions) were still pending,
    -- the connection aborted all of them.
    waitTillClosed conn
