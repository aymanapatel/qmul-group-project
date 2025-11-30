module Fetch (
    downloadRoads,
    downloadDisruptions
) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LBS

-- | Downloads road data from the TfL API.
--
-- Takes an API key as input and appends it to the request URL.
-- Returns the raw JSON response as a Lazy ByteString.
--
-- @
-- json <- downloadRoads "your_api_key"
-- @
downloadRoads :: String -- ^ The TfL API Key
              -> IO LBS.ByteString -- ^ The raw JSON response
downloadRoads apiKey = do
    let url = "https://api.tfl.gov.uk/Road?app_key=" ++ apiKey
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

-- | Downloads disruption data from the TfL API.
--
-- @
-- json <- downloadDisruptions "your_api_key"
-- @
downloadDisruptions :: String -- ^ The TfL API Key
                    -> IO LBS.ByteString -- ^ The raw JSON response
downloadDisruptions apiKey = do
    let url = "https://api.tfl.gov.uk/Road/All/Disruption?stripContent=false&app_key=" ++ apiKey
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response
