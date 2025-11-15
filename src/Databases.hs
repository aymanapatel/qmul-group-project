{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Databases (User(..), insertUser, getAllUsers, deleteAllUsers, database, initDB) where

import Control.Exception (bracket)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Data.Text (unpack)
import System.IO (hFlush, stdout)
import Fetch
import Types

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToRow User where
  toRow (User uid name email) = toRow (uid, name, email)

-- | Inserts a user into the database
insertUser :: Connection -> User -> IO ()
insertUser conn user = do
  execute conn "INSERT INTO users (id, name, email) VALUES (?, ?, ?)" user
  putStrLn $ "✓ Inserted user: " ++ show (name user)

-- | Retrieves all users from the database
getAllUsers :: Connection -> IO [User]
getAllUsers conn = query_ conn "SELECT id, name, email FROM users"

-- | Deletes all users from the database
deleteAllUsers :: Connection -> IO ()
deleteAllUsers conn = do
  execute_ conn "DELETE FROM users"
  putStrLn "✓ Cleared all users from database"

-- | Initializes the database
initDB :: Connection -> IO ()
initDB conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, email TEXT NOT NULL)"
  putStrLn "✓ Database initialized"

-- | Main database application
database :: IO ()
database = do
  putStrLn "Database opening connection"

  bracket (open "users.db") close $ \conn -> do
    initDB conn
    deleteAllUsers conn
    
    putStrLn "\nFetching users from JSONPlaceholder API..."
    
    mapM_ (fetchAndStore conn) [1..5]
    
    putStrLn "\nStored users in database:"
    users <- getAllUsers conn
    mapM_ printUser users
    
    putStrLn "\n✓ Project complete!"
  where
    fetchAndStore :: Connection -> Int -> IO ()
    fetchAndStore conn uid = do
      putStr $ "Fetching user " ++ show uid ++ "... "
      hFlush stdout
      maybeUser <- fetchUser uid
      case maybeUser of
        Just user -> insertUser conn user
        Nothing -> putStrLn "Failed to fetch user"

    printUser :: User -> IO ()
    printUser user = 
      putStrLn $ "  - " ++ unpack (name user) ++ 
                 " (" ++ unpack (email user) ++ ")"