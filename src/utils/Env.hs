module Utils.Env (
    loadEnv,
    getEnvVar
) where

import System.Directory (doesFileExist)
import Data.List.Split (splitOn)

-- | Loads environment variables from a .env file.
--
-- Checks if the .env file exists. If so, reads it and parses key-value pairs.
-- Returns a list of (Key, Value) tuples.
loadEnv :: IO [(String, String)]
loadEnv = do
    exists <- doesFileExist ".env"
    if exists
        then do
            content <- readFile ".env"
            return $ map parseLine (lines content)
        else return []
  where
    parseLine line = case splitOn "=" line of
        (key:val:_) -> (key, val)
        _ -> ("", "")

-- | Retrieves a specific environment variable by key.
--
-- Returns the value if found, or an empty string if not.
getEnvVar :: String -- ^ The key to look up
          -> [(String, String)] -- ^ The environment list
          -> String -- ^ The value found
getEnvVar key env = case lookup key env of
    Just val -> val
    Nothing -> ""
