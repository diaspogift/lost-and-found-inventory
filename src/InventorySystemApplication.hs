{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
module InventorySystemApplication
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


import DeclareLostItemDto


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
    "users" :> Get '[JSON] [User]
    :<|> "home" :> Get '[JSON] Welcome
    :<|> "lost-items" 
            :> ReqBody '[JSON] DeclareLostItemForm
            :> Post '[JSON] DeclareLostItemForm


startApp :: IO ()
startApp = run 8000 app





app :: Application
app = serve proxy routes

proxy :: Proxy API
proxy = Proxy

routes :: Server API
routes = 
    return users
    :<|> return welcomeMesg
    :<|> handlerDeclareLostItem




handlerDeclareLostItem :: 
    DeclareLostItemForm -> Handler DeclareLostItemForm
handlerDeclareLostItem declareLostItemForm = return declareLostItemForm

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

welcomeMesg = Welcome "Welcome home"
