{-# LANGUAGE OverloadedStrings #-}

module Page where

import Prelude
import qualified Prelude as P
import Data.Monoid (mempty, (<>))

import Data.Maybe
import Control.Monad

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

indexPage :: Maybe Bool -> Html
indexPage status = do
    docTypeHtml $ do
        H.head $ do
            H.title "LED CONTROL!"
            link ! rel "stylesheet" 
                 ! type_ "text/css" 
                 ! href "static/style.css"
        body $ do
            h1 "LED CONTROL!"
            when (isJust status) $ 
                h2 $ "Led is " <> if fromJust status then "on!" else "off!"
            H.form ! action "/" $ 
                button ! type_ "submit" 
                       ! class_ "button" 
                       ! name "status" 
                       ! value "true" $ "ON"
            H.form ! action "/" $ 
                button ! type_ "submit" 
                       ! class_ "button" 
                       ! name "status" 
                       ! value "false" $ "OFF" 
