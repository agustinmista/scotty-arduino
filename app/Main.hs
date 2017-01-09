{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Middleware.Static
import Web.Scotty
import System.Hardware.Arduino
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Data.Text
import Control.Conditional
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.MVar



import Page

main = do
    mvar <- newMVar False
    forkIO $ runArduino (handleLed mvar)
    forkIO $ runServer (app mvar)


----------------------------------------
-- Scotty
----------------------------------------
runServer = scotty 3000

optional p = param p `rescue` const next

app :: MVar Bool -> ScottyM ()
app mvar = do
    
    middleware $ staticPolicy noDots
    
    get "/" $ do
        status <- optional "status" 
        if status 
            then do 
                liftIO $ putMVar mvar True
                html $ renderHtml $ indexPage (Just status)
            else do 
                liftIO $ putMVar mvar False
                html $ renderHtml $ indexPage (Just status)
    
    get "/" $ html $ renderHtml $ indexPage Nothing
    

----------------------------------------
-- Arduino
----------------------------------------

handleLed :: MVar Bool -> Arduino ()
handleLed mvar = do
    setup
    forever $ do
        s <- liftIO (takeMVar mvar)
        setLed s 

runArduino = withArduino False "/dev/ttyUSB0"

led = digital 13
setup = setPinMode led OUTPUT
setLed = digitalWrite led


