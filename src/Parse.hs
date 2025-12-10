module Parse (
    parseRoads,
    parseDisruptions
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Types

-- | Parses JSON data into a list of 'Road' objects.
--
-- Uses Aeson's 'eitherDecode' to parse the Lazy ByteString.
-- Returns 'Left errorMsg' on failure, or 'Right [Road]' on success.
parseRoads :: LBS.ByteString -- ^ The raw JSON data
           -> Either String [Road] -- ^ The result of parsing
parseRoads json = eitherDecode json

-- | Parses JSON data into a list of 'Disruption' objects.
--
-- Uses Aeson's 'eitherDecode' to parse the Lazy ByteString.
-- Returns 'Left errorMsg' on failure, or 'Right [Disruption]' on success.
parseDisruptions :: LBS.ByteString -- ^ The raw JSON data
                 -> Either String [Disruption] -- ^ The result of parsing
parseDisruptions json = eitherDecode json
