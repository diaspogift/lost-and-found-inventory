{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
module InventorySystemApplication
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
import DeclaredLostItemHandler --- ????????? should not be here
import Control.Monad.Except



------
import Data.Text.Lazy (Text)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)
  
$(deriveJSON defaultOptions ''User)

data Welcome = Welcome {
    message :: String
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Welcome)





--------





--------

type API = 
    "home" :> Get '[JSON] Welcome
    :<|> "lost-items" 
            :> ReqBody '[JSON] DeclareLostItemForm
            :> Post '[JSON] Resp


startApp :: IO ()
startApp = run 8000 app





app :: Application
app = serve proxy routes

proxy :: Proxy API
proxy = Proxy

routes :: Server API
routes = 
    return welcomeMesg
    :<|> handlerDeclareLostItem



handlerDeclareLostItem :: 
    DeclareLostItemForm -> Handler Resp
handlerDeclareLostItem declareLostItemForm = 
    do
        -- setting up the declare lost item command

        let unvalidatedLostItem = toUnvalidatedLostItem declareLostItemForm
            declareLostItemCmd = Register (Command unvalidatedLostItem "2019 10 10 12:34:56" "111111111111111111111111111111111111")

        -- calling the command handler and lifting the result into the Handler Tranformer 

        res <- liftIO $ runExceptT $ handle declareLostItemCmd

        -- Handling the response

        case res of 
            Right events -> 
                let resp = fmap fromDomain events
                in return $ Success resp
            Left error -> 
                let resp = fromDeclareLostItemError error
                in return $ Error resp



welcomeMesg = Welcome "LOST |&| FOUND: INVENTORY"
