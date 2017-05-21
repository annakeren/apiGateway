{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T
import Data.String
import PutJSON
import JSON

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)


main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8081 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
            text (T.pack (renderJValue (JObject [("Account", JNumber 1), ("Open", JBool True)])))
       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text (T.pack (renderJValue (JObject [("Hello", JString name), ("you are visitor number", JInt visitorNumber)])))