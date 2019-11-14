{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module InventoryAPI
  ( startApp,
    app,
  )
where

import CommonDtos
import CommonSimpleTypes
import Control.Monad.Except
import CreateAttributeDto
import CreateAttributePublicTypes
import CreateCategoryCommonDto
import CreateRootCategoryDto
import CreateRootCategoryPublicTypes
import CreateSubCategoryDto
import CreateSubCategoryPublicTypes
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.TH
import Data.ByteString.Lazy.Char8
import DeclareLostItemDto
import DeclaredLostItemPublicTypes
import GHC.Generics
import InventorySystemCommands
import InventorySystemCommandsHandler
import Network.Wai
import Network.Wai.Handler.Warp
import Servant




-- ==========================================================================
-- This file contains the definitions of the Inventory sub-system Rest API
-- exposed at the boundary of the bounded context
-- ==========================================================================




-- --------------------------------------------------------------------------
-- API helper / return types
-- --------------------------------------------------------------------------




newtype Welcome
  = Welcome
      { greeting :: String
      }
  deriving (Generic, Eq, Show)

instance ToJSON Welcome

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
    :<|> "root-categories"
      :> ReqBody '[JSON] CreateRootCategoryForm
      :> Post '[JSON] RestCallResponse
    :<|> "sub-categories"
      :> ReqBody '[JSON] CreateSubCategoryForm
      :> Post '[JSON] RestCallResponse




data RestCallResponse
  = DclLstItemResp RespDclLstItemWorkflow
  | CrtAttrRefResp RespCrtAttrRefWorkflow
  | CrtRooCatgrResp RespCrtCatgrWorkflow
  | CrtSubCatgrResp RespCrtCatgrWorkflow
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
    :<|> handleCreateRootCategory
    :<|> handleCreateSubCategory




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
        declareLostItemCmd = Register 
            (Command 
                unvalidatedLostItem 
                "2019 10 10 12:34:56" 
                "111111111111111111111111111111111111"
            )
    -- calling the command handler and lifting the result into the Handler Transformer

    res <- (liftIO . runExceptT) $ handle declareLostItemCmd
    -- Handling the response

    case res of
      Right (DclreLostItemEvt events) ->
        return $ DclLstItemResp $ fromDclLstItmEvtDomain <$> events
      Left (DclreLostItemErr error) ->
        let errorMsg = fromWorkflowError error
            cd = worflowErrorCde errorMsg
            msg = worflowErrorMsg errorMsg
            body = cd ++ ": " ++ msg
         in case error of
              Validation (ValidationError err) ->
                throwError $ toServantError 400 cd msg
              Domain (DomainError err) ->
                throwError $ toServantError 406 cd msg
              DataBase (DataBaseError err) ->
                throwError $ toServantError 404 cd msg
              Remote err ->
                throwError $ toServantError 404 cd msg




handlerCreateAttributeRef ::
  CreateAttributeRefForm -> Handler RestCallResponse
handlerCreateAttributeRef attributeRefForm =
  do
    -- setting up the create attribute command

    let unvalidatedAttributeRef = toUnvalidatedAttributeRef attributeRefForm
        createAttrRefCmd = CreateAttribute 
            (Command 
                unvalidatedAttributeRef 
                "2019 10 10 12:34:56" 
                "111111111111111111111111111111111111"
            )
    -- calling the command handler and lifting the result into the Handler Transformer

    res <- (liftIO . runExceptT) $ handle createAttrRefCmd
    -- Handling the response

    case res of
      Right (CrteAttribueEvt events) ->
        return $ CrtAttrRefResp $ fromCrtAttrEvtDomain <$> events
      Left (CrteAttribueErr error) ->
        let errorMsg = fromWorkflowError error
            cd = worflowErrorCde errorMsg
            msg = worflowErrorMsg errorMsg
            body = cd ++ ": " ++ msg
         in case error of
              Validation (ValidationError err) ->
                throwError $ toServantError 400 cd msg
              Domain (DomainError err) ->
                throwError $ toServantError 406 cd msg
              DataBase (DataBaseError err) ->
                throwError $ toServantError 404 cd msg
              Remote err ->
                throwError $ toServantError 400 cd msg





handleCreateRootCategory :: CreateRootCategoryForm -> Handler RestCallResponse
handleCreateRootCategory rootCategoryForm =
  do
    -- setting up the create attribute command

    let unvalidatedRootCategory = toUnvalidatedRootCategory rootCategoryForm
        createRootCategoryCmd = CreateRootCategory 
            (Command 
                unvalidatedRootCategory 
                "2019 10 10 12:34:56" 
                "111111111111111111111111111111111111"
            )
    -- calling the command handler and lifting the result into the Handler Transformer

    res <- liftIO . runExceptT . handle $ createRootCategoryCmd
    -- Handling the response

    case res of
      Right (CrteRootCatgrEvt events) ->
        return $ CrtRooCatgrResp $ fromCrtCatgrEvtDomain <$> events
      Left (CrteRootCatgrErr error) ->
        let errorMsg = fromWorkflowError error
            cd = worflowErrorCde errorMsg
            msg = worflowErrorMsg errorMsg
            body = cd ++ ": " ++ msg
         in case error of
              Validation (ValidationError err) ->
                throwError $ toServantError 400 cd msg
              Domain (DomainError err) ->
                throwError $ toServantError 406 cd msg
              DataBase (DataBaseError err) ->
                throwError $ toServantError 404 cd msg
              Remote err ->
                throwError $ toServantError 400 cd msg





handleCreateSubCategory ::CreateSubCategoryForm -> Handler RestCallResponse
handleCreateSubCategory subCategoryForm =
  do
    -- setting up the create sub catergory command

    let unvalidatedSubCategory = toUnvalidatedSubCategory subCategoryForm
        createSubCategoryCmd = CreateSubCategory (
                Command 
                    unvalidatedSubCategory 
                    "2019 10 10 12:34:56" 
                    "111111111111111111111111111111111111"
                )
    -- calling the command handler and lifting the result into the Handler Transformer

    res <- liftIO . runExceptT . handle $ createSubCategoryCmd
    -- Handling the response

    case res of
      Right (CrteSubCatgrEvt events) ->
        return $ CrtSubCatgrResp $ fromCrtCatgrEvtDomain <$> events
      Left (CrteSubCatgrErr error) ->
        let errorMsg = fromWorkflowError error
            cd = worflowErrorCde errorMsg
            msg = worflowErrorMsg errorMsg
            body = cd ++ ": " ++ msg
         in case error of
              Validation (ValidationError err) ->
                throwError $ toServantError 400 cd msg
              Domain (DomainError err) ->
                throwError $ toServantError 406 cd msg
              DataBase (DataBaseError err) ->
                throwError $ toServantError 404 cd msg
              Remote err ->
                throwError $ toServantError 400 cd msg

toServantError :: Int -> String -> String -> ServerError
toServantError code reason body =
  ServerError
    { errHTTPCode = code,
      errReasonPhrase = reason,
      errBody = pack body,
      errHeaders = []
    }
