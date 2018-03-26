{-# LANGUAGE OverloadedStrings #-}
module Website (
createTable,
getAllWebsites,
find,
newWebsite,
WebsiteField,
getUpdatedWebsites,
getUpdatedWebsiteUrls,
updateWebsites
) where

import Control.Applicative()
import Control.Monad (filterM)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow()
import Database.SQLite.Simple.ToRow()
import Database.SQLite.Simple.ToField()
import Data.Time (UTCTime, getCurrentTime)
import Data.Int()
import Scraper (websiteHash)


data WebsiteField = WebsiteField { i :: Int,
                                   u :: String,
                                   h :: Maybe Int,
                                   lu :: String
                                 } deriving (Show)

data Parameter = Hash Int | URL String deriving (Show)

data Website = Website { url :: String,
                         hash :: Maybe Int,
                         last_updated :: UTCTime
                       }

instance FromRow WebsiteField where
  fromRow = WebsiteField <$> field <*> field <*> field <*> field


tableCreateQuery :: Query
tableCreateQuery = "CREATE TABLE IF NOT EXISTS websites (id INTEGER PRIMARY KEY, url TEXT, hash INTEGER, last_updated TIMESTAMP)"

createTable :: IO ()
createTable = do
  conn <- open "sitewatcher.db"
  execute_ conn tableCreateQuery

getAllWebsites :: IO [WebsiteField]
getAllWebsites = do
  conn <- open "sitewatcher.db"
  r <- query_ conn "SELECT * FROM websites" :: IO [WebsiteField]
  close conn
  return r

insertWebsite :: Website -> IO ()
insertWebsite newWebsite = do
  conn <- open "sitewatcher.db"
  let new_url = url newWebsite
  let new_hash = hash newWebsite
  let new_last_updated = last_updated newWebsite
  let query = "INSERT INTO websites (url, hash, last_updated) VALUES (?, ?, ?)"
  execute conn query (new_url, new_hash, new_last_updated)
  close conn

newWebsite :: String -> IO ()
newWebsite url = do
  hash <- websiteHash url
  last_updated <- getCurrentTime
  let new_website = Website{url=url, hash=hash, last_updated=last_updated}
  insertWebsite new_website

findById :: Integer -> IO WebsiteField
findById id = do
  conn <- open "sitewatcher.db"
  let q = "SELECT * FROM websites WHERE id=?"
  rows <- query conn q (Only (id :: Integer)) :: IO [WebsiteField]
  close conn
  return $ head rows

updateWebsite :: WebsiteField -> IO ()
updateWebsite wf = do
  conn <- open "sitewatcher.db"
  new_hash <- websiteHash $ u wf
  current_time <- getCurrentTime
  let q = "UPDATE websites SET hash=?, last_updated=? WHERE id=?"
  execute conn q (new_hash, current_time, i wf)

updateWebsites :: [WebsiteField] -> IO ()
updateWebsites wfs = foldMap updateWebsite wfs

checkIfUpdated :: WebsiteField -> IO Bool
checkIfUpdated wf = do
  new_hash <- websiteHash $ u wf
  return $ not (old_hash == new_hash)
  where
    old_hash = h wf
    new_hash = websiteHash $ u wf

getUpdatedWebsites :: IO [WebsiteField]
getUpdatedWebsites = do
  all_websites <- getAllWebsites
  filterM checkIfUpdated all_websites

getUpdatedWebsiteUrls :: IO [String]
getUpdatedWebsiteUrls = do
  updated_websites <- getUpdatedWebsites
  return $ map (\wf -> u wf) updated_websites

find :: Parameter -> IO [WebsiteField]
find (Hash hash) = do
  conn <- open "test.db"
  let q = "SELECT * from websites where hash=?"
  rows <- query conn q (Only (hash :: Int)) :: IO [WebsiteField]
  close conn
  return rows
find (URL url) = do
  conn <- open "test.db"
  let q = "SELECT * from websites where url=?"
  rows <- query conn q (Only (url :: String)) :: IO [WebsiteField]
  close conn
  return rows
