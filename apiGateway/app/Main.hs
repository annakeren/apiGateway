{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Data.Aeson  hiding (json)
import Data.Monoid ((<>))
import Data.Text  (Text, pack)
import GHC.Generics
import Data.IORef

data Account = Account
  { email :: Text
  , password  :: Text
  } deriving (Generic, Show)
instance ToJSON Account
instance FromJSON Account

data Product = Product
  { name :: Text
  , price  :: Int
  } deriving (Generic, Show)
instance ToJSON Product
instance FromJSON Product

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main =
    do
        spockCfg <- defaultSpockCfg () PCNoDatabase ()
        runSpock 8080 (spock spockCfg app)

app :: Api
app =
    do get root $
           text "Hello World!"
       get "products" $ do
            json $ Product { name = "chocolate", price = 2 }
       get "account" $
            redirect "http://localhost:8081"
       post "account/open" $ do
            theAccount <- jsonBody' :: ApiAction Account
            text $ "Parsed: " <> pack (show theAccount)
