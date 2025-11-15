{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Fetch (fetchUser) where

import Data.Aeson (FromJSON(..), ToJSON(..), decode, withObject, (.:))
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Simple
import Types
import Data.Text (pack)

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User
    <$> v .: "id"
    <*> (pack <$> v .: "name")
    <*> (pack <$> v .: "email")

instance ToJSON User where
  toJSON u = undefined

-- HTTP operations
fetchUser :: Int -> IO (Maybe User)
fetchUser uid = do
  putStrLn $ "Fetching user " ++ show uid ++ "..."
  let url = "https://jsonplaceholder.typicode.com/users/" ++ show uid
  request <- parseRequest url
  response <- httpLBS request
  let body = getResponseBody response
  case decode body of
    Just jsonUser -> do
      putStrLn $ "✓ Fetched: " ++ show jsonUser
      return $ Just jsonUser
    Nothing -> do
      putStrLn "✗ Failed to decode JSON"
      return Nothing