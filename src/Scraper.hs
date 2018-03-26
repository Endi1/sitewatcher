{-# LANGUAGE OverloadedStrings #-}

module Scraper (
websiteHash
) where

import Text.HTML.Scalpel
import Network.Curl
import Data.Hashable

getRawWebsiteData :: String -> IO String
getRawWebsiteData url = do
  result <- curlGetString url []
  if fst result == CurlOK
    then return $ snd result
    else return ""

websiteBody :: String -> IO (Maybe String)
websiteBody url = do
  rawData <- getRawWebsiteData url
  return $ scrapeStringLike rawData body
  where
    body :: Scraper String String
    body = text "body"

websiteHash :: String -> IO (Maybe Int)
websiteHash url = do
  body <- websiteBody url
  return $ pure (hash body)
