{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)


import           Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

main :: IO ()
main = simpleHTTP nullConf $ ok $ toResponse $ appTemplate (H.h1 "hello, blaze!")

appTemplate :: H.Html -> H.Html
appTemplate body =
    H.html $ do
        H.head $ do
            H.title (H.toHtml ("Fluid simulation" :: String))
            H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
            H.meta ! A.name "keywords" ! A.content "happstack, blaze, html"
            H.body $ do
                body
