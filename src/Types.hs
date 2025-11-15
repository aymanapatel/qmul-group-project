{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (User(..)) where
import Data.Text (Text)
import GHC.Generics (Generic)

-- | User data type representing a user from JSONPlaceholder API
data User = User
  { id :: Int
  , name :: Text
  , email :: Text
  } deriving (Show, Generic)

