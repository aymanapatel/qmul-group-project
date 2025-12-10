module Fetch (
    downloadRoads,
    downloadDisruptions
) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (try, Exception)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), responseStatus)
import Network.HTTP.Types.Status (statusCode)

-- | Downloads road data from the TfL API.
--
-- Takes an API key as input and appends it to the request URL.
-- Returns the raw JSON response as a Lazy ByteString or an error message.
downloadRoads :: String -- ^ The TfL API Key
              -> IO (Either String LBS.ByteString) -- ^ The raw JSON response or error
downloadRoads apiKey = do
    let url = "https://api.tfl.gov.uk/Road?app_key=" ++ apiKey
    downloadWithHandler url

-- | Downloads disruption data from the TfL API.
--
-- Returns the raw JSON response as a Lazy ByteString or an error message.
downloadDisruptions :: String -- ^ The TfL API Key
                    -> IO (Either String LBS.ByteString) -- ^ The raw JSON response or error
downloadDisruptions apiKey = do
    let url = "https://api.tfl.gov.uk/Road/All/Disruption?stripContent=false&app_key=" ++ apiKey
    downloadWithHandler url

-- | Helper function to perform the HTTP request with error handling.
downloadWithHandler :: String -> IO (Either String LBS.ByteString)
downloadWithHandler url = do
    request <- parseRequest url
    result <- try (httpLBS request) :: IO (Either HttpException (Response LBS.ByteString))
    case result of
        Left ex -> return $ Left (handleHttpException ex)
        Right response -> do
            let status = statusCode (getResponseStatus response)
            if status >= 200 && status < 300
                then return $ Right (getResponseBody response)
                else return $ Left (handleStatusCode status)

-- | Handles HTTP exceptions and converts them to user-friendly error messages.
handleHttpException :: HttpException -> String
handleHttpException (HttpExceptionRequest _ content) = case content of
    StatusCodeException response _ -> 
        let status = statusCode (responseStatus response)
        in handleStatusCode status
    ConnectionTimeout -> "Error: Connection timed out. Please check your internet connection."
    ConnectionFailure _ -> "Error: Failed to connect to the server. Please check your internet connection."
    _ -> "Error: An unexpected network error occurred: " ++ show content
handleHttpException (InvalidUrlException _ _) = "Error: Invalid URL constructed."

-- | Handles specific HTTP status codes.
handleStatusCode :: Int -> String
handleStatusCode 401 = "Error: Unauthorized (401). Please check if your API key is valid and active."
handleStatusCode 403 = "Error: Forbidden (403). You do not have permission to access this resource."
handleStatusCode 404 = "Error: Not Found (404). The requested resource could not be found."
handleStatusCode 429 = "Error: Too Many Requests (429). You have exceeded your API rate limit."
handleStatusCode 500 = "Error: Internal Server Error (500). The TfL server encountered an error."
handleStatusCode 502 = "Error: Bad Gateway (502). The server received an invalid response from an upstream server."
handleStatusCode 503 = "Error: Service Unavailable (503). The TfL API is currently unavailable."
handleStatusCode 504 = "Error: Gateway Timeout (504). The upstream server failed to respond in time."
handleStatusCode code = "Error: HTTP request failed with status code " ++ show code 
