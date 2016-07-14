{-# LANGUAGE OverloadedStrings #-}

module Pages where

import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.Format
import           Prelude          hiding (head)
import           Text.Blaze.Html5

frontPage :: Int -> Int -> UTCTime -> Html
frontPage a b t = do
  docType
  head $ title "this is a front page!!"
  body $ do
    p "this is a paragraph!!"
    p ("we got an integer: " <> toHtml a)
    p ("and another: " <> toHtml b)
    let time = formatTime defaultTimeLocale rfc822DateFormat t
    p ("request was served at " <> toHtml time)
