-- filepath: [Main.hs](http://_vscodecontentref_/1)
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.IO (hFlush, stdout)
import Databases
import System.Environment (getArgs)





-- | Main application
main :: IO ()
main = do
  putStrLn "\n=== Haskell Stack Project: TFL App Group 27 ==="
  putStrLn ""
  
  args <- getArgs
  case args of 
    -- TODO
    -- ("create":_) -> Creates sqlite database and tables
    -- ("run":_) -> Downloads data from API and saves to database ...
    -- ("dumpdata":_) -> Generates `data.json` with all DB data...
    ("database":_) -> database
    _ -> putStrLn "Usage: <exe> [database]"


