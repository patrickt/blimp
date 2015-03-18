{-# LANGUAGE OverloadedStrings #-}

module Pages where

import           Data.Monoid
import           Prelude hiding (head)
import           Text.Blaze.Html5

frontPage :: Int -> Int -> Html
frontPage a b = do
  docType
  head $ title "this is a front page!!"
  body $ do
    p "this is a paragraph!!"
    p ("we got an integer: " <> toHtml a)
    p ("and another: " <> toHtml b)
