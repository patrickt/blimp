{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Airship
import           Airship.Helpers
import           Airship.Resource
import           Airship.Route
import           Airship.Types

import           Control.Applicative
import           Data.Pool
import           Data.IORef
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html.Renderer.Utf8

import           Network.Wai (strictRequestBody)
import           Network.Wai.Handler.Warp ( runSettings
                                          , defaultSettings
                                          , setPort
                                          , setHost
                                          )


import           Pages

data AppState = AppState
            { pool :: Pool Int
            } deriving (Show)

buildHtmlResponse :: Html -> ResponseBody m
buildHtmlResponse = ResponseBuilder . renderHtmlBuilder

rootResource :: Resource AppState IO
rootResource = defaultResource
               { knownContentType = contentTypeMatches ["text/*"]
               , contentTypesProvided = do
                  p <- pool <$> getState
                  withResource p $ \int1 ->
                    withResource p $ \int2 -> do
                      now <- requestTime
                      let page = frontPage int1 int2 now
                      return [ ("text/html", return (buildHtmlResponse page))]
               }

generateInt :: IORef Int -> IO Int
generateInt r = do
  val <- readIORef r
  modifyIORef r succ
  return val

destroyInt :: IORef Int -> Int -> IO ()
destroyInt r _ = modifyIORef r succ

routes :: RoutingSpec AppState IO ()
routes = do
  root #> rootResource

main :: IO ()
main = do
  let port = 3000
      host = "127.0.0.1"
      settings = setPort port (setHost host defaultSettings)
  counter <- newIORef 0
  pool <- createPool (generateInt counter) (destroyInt counter) 10 60 10
  let state = AppState pool
  putStrLn "listening on port 3000"
  runSettings settings (resourceToWai routes defaultResource state)
