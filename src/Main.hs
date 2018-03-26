{-# LANGUAGE OverloadedStrings #-}
module Main where

import Website(newWebsite, getUpdatedWebsites, getUpdatedWebsiteUrls, createTable, updateWebsites)

getInput :: IO ()
getInput = do
  putStrLn "Would you like to insert a new website? [Y/n]"
  answer <- getLine
  if parseNewWebsiteQuestion answer
    then createWebsite
    else putStrLn "Goodbye!"

parseNewWebsiteQuestion :: String -> Bool
parseNewWebsiteQuestion "y" = True
parseNewWebsiteQuestion "" = True
parseNewWebsiteQuestion _ = False

createWebsite :: IO ()
createWebsite = do
  putStrLn "URL:"
  url <- getLine
  putStrLn url
  newWebsite url

showUpdatedWebsites :: IO ()
showUpdatedWebsites = do
  updatedUrls <- getUpdatedWebsiteUrls
  updated_websites <- getUpdatedWebsites
  updateWebsites updated_websites
  print updatedUrls

main :: IO ()
main = createTable
