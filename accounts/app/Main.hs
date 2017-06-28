{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import qualified Data.Text as T
import Data.String
import PutJSON
import JSON
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

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)


type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main =
    do
        spockCfg <- defaultSpockCfg () PCNoDatabase ()
        runSpock 8081 (spock spockCfg app)

--app :: SpockM () MySession MyAppState ()
app :: Api
app =
    do get root $
            text (T.pack (renderJValue (JObject [("Account", JNumber 1), ("Open", JBool True)])))
--       get ("hello" <//> var) $ \name ->
--           do (DummyAppState ref) <- getState
--              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
--              text (T.pack (renderJValue (JObject [("Hello", JString name), ("you are visitor number", JInt visitorNumber)])))
       post "account/open" $ do
           theAccount <- jsonBody' :: ApiAction Account
           text $ "Parsed: " <> pack (show theAccount)