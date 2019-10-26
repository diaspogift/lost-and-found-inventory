{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric #-}

module InventoryAPI
    ( startApp
    , app
    ) where

import Data.Aeson hiding (Success, Error)
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import CommonSimpleTypes
import CommonDtos


import InventorySystemCommands
import InventorySystemCommandsHandler

import DeclareLostItemDto
import  DeclaredLostItemPublicTypes 

import CreateAttributeDto
import CreateAttributePublicTypes

import Control.Monad.Except
import Data.ByteString.Lazy.Char8

import GHC.Generics





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
            :> Post '[JSON] RestCallResponse
    :<|> "attributes"
            :> ReqBody '[JSON] CreateAttributeRefForm
            :> Post '[JSON] RestCallResponse



data RestCallResponse =  
      DclLstItemResp RespDclLstItemWorkflow 
    | CrtAttrRefResp RespCrtAttrRefWorkflow
    deriving (Generic, Show)


instance ToJSON RestCallResponse

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
    :<|> handlerCreateAttributeRef



-- --------------------------------------------------------------------------
-- Routes handler functions
-- --------------------------------------------------------------------------

--- TODO: Need to return proper error codes :) No hack allowed 
---
---

handlerDeclareLostItem :: 
    DeclareLostItemForm -> Handler RestCallResponse
handlerDeclareLostItem declareLostItemForm = 
    do
        -- setting up the declare lost item command

        let unvalidatedLostItem = toUnvalidatedLostItem declareLostItemForm
            declareLostItemCmd = Register (Command unvalidatedLostItem "2019 10 10 12:34:56" "111111111111111111111111111111111111")

        -- calling the command handler and lifting the result into the Handler Transformer 

        res <- (liftIO . runExceptT) $ handle declareLostItemCmd

        -- Handling the response

        case res of 
            Right (DclreLostItemEvt events) -> 
                return $ DclLstItemResp $ fmap fromDclLstItmEvtDomain events
            Left (DclreLostItemErr error) -> 
                let errorMsg = fromWorkflowError error
                    cd = code errorMsg
                    msg = message errorMsg
                    body = cd ++ ": " ++ msg
                in 
                    case error of
                        Validation (ValidationError err) ->
                            throwError $ toServantError 400 cd msg
                        Db (DbError err) ->
                            throwError $ toServantError 404 cd msg
                        Remote err ->
                            throwError $ toServantError 404 cd msg



                        

handlerCreateAttributeRef :: 
    CreateAttributeRefForm -> Handler RestCallResponse
handlerCreateAttributeRef attributeRefForm = 
    do
        -- setting up the create attribute command

        let unvalidatedAttributeRef = toUnvalidatedAttributeRef attributeRefForm
            createAttrRefCmd = CreateAttribute (Command unvalidatedAttributeRef "2019 10 10 12:34:56" "111111111111111111111111111111111111")

        -- calling the command handler and lifting the result into the Handler Transformer 

        res <- (liftIO . runExceptT) $ handle createAttrRefCmd

        -- Handling the response

        case res of 
            Right (CrteAttribueEvt events) -> 
                return $ CrtAttrRefResp $ fmap fromCrtAttrEvtDomain events 
            Left (CrteAttribueErr error) -> 
                let errorMsg = fromWorkflowError error
                    cd = code errorMsg
                    msg = message errorMsg
                    body = cd ++ ": " ++ msg
                in
                    case error of
                        Validation (ValidationError err) ->
                            throwError $ toServantError 400 cd msg
                        Db (DbError err) ->
                            throwError $ toServantError 404 cd msg
                        Remote err ->
                            throwError $ toServantError 400 cd msg



toServantError :: Int -> String -> String -> ServerError
toServantError code reason body = 
    ServerError {
        errHTTPCode = code ,
        errReasonPhrase = reason,
        errBody = pack body,
        errHeaders = []
        }
                                    