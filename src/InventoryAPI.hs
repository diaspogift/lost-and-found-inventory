{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
module InventoryAPI
    ( startApp
    , app
    ) where

import Data.Aeson hiding (Success, Error)
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


import DeclareLostItemDto
import InventorySystemCommands
import InventorySystemCommandsHandler
import DeclaredLostItemPublicTypes

import Control.Monad.Except
import Data.ByteString.Lazy.Char8




-- ==========================================================================
-- This file contains the definitions of the Inventory sub-system Rest API 
-- exposed at the boundary of the bounded context
-- ==========================================================================



-- --------------------------------------------------------------------------
-- API helper / return types
-- --------------------------------------------------------------------------


data Welcome = Welcome {
    greeting :: String
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Welcome)

welcomeMesg = Welcome "LOST |&| FOUND: INVENTORY"


-- --------------------------------------------------------------------------
-- API types
-- --------------------------------------------------------------------------


type API = 
    "home" :> Get '[JSON] Welcome
    :<|> "lost-items" 
            :> ReqBody '[JSON] DeclareLostItemForm
            :> Post '[JSON] Resp



-- --------------------------------------------------------------------------
-- API server
-- --------------------------------------------------------------------------


startApp :: IO ()
startApp = run 8000 app


app :: Application
app = serve proxy routes


proxy :: Proxy API
proxy = Proxy


-- --------------------------------------------------------------------------
-- API routes
-- --------------------------------------------------------------------------


routes :: Server API
routes = 
    return welcomeMesg
    :<|> handlerDeclareLostItem



-- --------------------------------------------------------------------------
-- Routes handler functions
-- --------------------------------------------------------------------------

--- TODO: Need to return proper error codes :) No hack allowed 
---
---

handlerDeclareLostItem :: 
    DeclareLostItemForm -> Handler Resp
handlerDeclareLostItem declareLostItemForm = 
    do
        -- setting up the declare lost item command

        let unvalidatedLostItem = toUnvalidatedLostItem declareLostItemForm
            declareLostItemCmd = Register (Command unvalidatedLostItem "2019 10 10 12:34:56" "111111111111111111111111111111111111")

        -- calling the command handler and lifting the result into the Handler Transformer 

        res <- (liftIO . runExceptT) $ handle declareLostItemCmd

        -- Handling the response

        case res of 
            Right events -> 
                let resp = fmap fromDomain events
                in return $ Success resp
            Left error -> 
                case error of
                    Validation (ValidationError err) ->
                        let errorMsg = fromDeclareLostItemError error
                            cd = code errorMsg
                            msg = message errorMsg
                            body = cd ++ ": " ++ msg
                            servantError = 
                                ServerError {
                                    errHTTPCode = 400 ,
                                    errReasonPhrase = code errorMsg,
                                    errBody = (pack . message) errorMsg,
                                    errHeaders = []
                                    }

                        in throwError servantError
                    Db (DbError err) ->
                        let errorMsg = fromDeclareLostItemError error
                            cd = code errorMsg
                            msg = message errorMsg
                            body = cd ++ ": " ++ msg
                            servantError = 
                                ServerError {
                                    errHTTPCode = 404 ,
                                    errReasonPhrase = code errorMsg,
                                    errBody = (pack . message) errorMsg,
                                    errHeaders = []
                                    }

                        in throwError servantError

                    Remote err ->
                        let errorMsg = fromDeclareLostItemError error
                            cd = code errorMsg
                            msg = message errorMsg
                            body = cd ++ ": " ++ msg
                            servantError = 
                                ServerError {
                                    errHTTPCode = 400 ,
                                    errReasonPhrase = code errorMsg,
                                    errBody = (pack . message) errorMsg,
                                    errHeaders = []
                                    }

                        in throwError servantError


                         




