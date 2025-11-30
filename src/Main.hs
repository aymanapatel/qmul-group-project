module Main (main) where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (encode)
import Types
import Fetch
import Parse
import Database
import Text.Printf (printf)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import Data.List.Split (splitOn)
import qualified Data.Text as T

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

main :: IO ()
main = do
    args <- getArgs
    env <- loadEnv
    let apiKey = getEnvVar "TFL_APP_KEY" env
    
    case args of
        ["create"] -> do
            putStrLn "Creating database tables..."
            createTables
            putStrLn "Done."
            
        ["loaddata"] -> do
            if null apiKey
                then putStrLn "Error: TFL_APP_KEY not found in .env"
                else do
                    putStrLn "Downloading road data..."
                    jsonRoads <- downloadRoads apiKey
                    case parseRoads jsonRoads of
                        Left err -> putStrLn $ "Error parsing roads: " ++ err
                        Right roads -> do
                            putStrLn $ "Saving " ++ show (length roads) ++ " roads to database..."
                            saveRoads roads
                            
                    putStrLn "Downloading disruption data..."
                    jsonDisruptions <- downloadDisruptions apiKey
                    case parseDisruptions jsonDisruptions of
                        Left err -> putStrLn $ "Error parsing disruptions: " ++ err
                        Right disruptions -> do
                            putStrLn $ "Saving " ++ show (length disruptions) ++ " disruptions to database..."
                            saveDisruptions disruptions
                    putStrLn "Done."
            
        ["dumpdata"] -> do
            putStrLn "Dumping data to data.json..."
            logs <- dumpLogs
            LBS.writeFile "data.json" (encode logs)
            putStrLn "Done."
            
        ["search"] -> searchLoop

        ["report"] -> do
            putStrLn "Generating Road Reliability Report..."
            report <- getReliabilityReport
            putStrLn "----------------------------------------------------------------"
            printf "%-25s | %-10s | %-10s | %-10s\n" "Road Name" "Total Logs" "Good Svc" "Reliability"
            putStrLn "----------------------------------------------------------------"
            mapM_ (\(name, total, good, reliability) -> 
                printf "%-25s | %-10d | %-10d | %-9.1f%%\n" name total good reliability) report
            putStrLn "----------------------------------------------------------------"
            
            putStrLn "\nTraffic Trend Analysis (Worst Disruption Times):"
            
            putStrLn "\nBy Day of Week:"
            putStrLn "----------------------"
            days <- getWorstDayAnalysis
            if null days
                then putStrLn "No disruption data available."
                else mapM_ (\(day, count) -> printf "%-10s: %d disruptions\n" (T.unpack day) count) days
                
            putStrLn "\nBy Hour of Day:"
            putStrLn "----------------------"
            hours <- getWorstHourAnalysis
            if null hours
                then putStrLn "No disruption data available."
                else mapM_ (\(hour, count) -> printf "%-10s: %d disruptions\n" (T.unpack hour) count) hours
            
        _ -> putStrLn "Usage: stack run -- [create|loaddata|dumpdata|report|search]"

searchLoop :: IO ()
searchLoop = do
    putStrLn "\n--- Search Menu ---"
    putStrLn "1. Search by road name"
    putStrLn "2. Search by severity level"
    putStrLn "q. Quit"
    putStrLn "Enter option:"
    option <- getLine
    case option of
        "1" -> searchByName
        "2" -> searchBySeverity
        "q" -> return ()
        _ -> do
            putStrLn "Invalid option."
            searchLoop

searchByName :: IO ()
searchByName = do
    putStrLn "\nEnter road name or '#' to go back:"
    query <- getLine
    case query of
        "#" -> searchLoop
        _ -> do
            results <- searchRoads (T.pack query)
            case results of
                [] -> do
                    putStrLn "No Roads found by this name. Showing all roads:"
                    allRoads <- getAllRoads
                    displayResults allRoads
                
                [(rid, name)] -> do
                    putStrLn $ "\nFound one road: " ++ T.unpack name
                    printRoadStatus rid
                    promptContinuation
                    
                _ -> displayResults results

searchBySeverity :: IO ()
searchBySeverity = do
    putStrLn "\n--- Severity Search Menu ---"
    putStrLn "1. Good"
    putStrLn "2. Serious"
    putStrLn "3. Severe"
    putStrLn "#. Back to Main Menu"
    putStrLn "q. Quit"
    putStrLn "Enter option:"
    option <- getLine
    case option of
        "1" -> performSeveritySearch "Good"
        "2" -> performSeveritySearch "Serious"
        "3" -> performSeveritySearch "Severe"
        "#" -> searchLoop
        "*" -> searchLoop
        "q" -> return ()
        _ -> do
            putStrLn "Invalid option."
            searchBySeverity

performSeveritySearch :: String -> IO ()
performSeveritySearch severity = do
    results <- getRoadsBySeverity (T.pack severity)
    if null results
        then do
            putStrLn $ "No roads found with severity: " ++ severity
            searchBySeverity
        else do
            putStrLn $ "\nRoads with severity: " ++ severity
            printf "%-5s %-30s %-25s %-30s\n" "No." "Road Name" "Timing" "Description"
            putStrLn $ replicate 90 '-'
            mapM_ (\(i, (_, name, desc, time)) -> printf "%-5d %-30s %-25s %-30s\n" (i :: Int) name time desc) (zip [1..] results)
            
            putStrLn "\n#. Back to Main Menu"
            putStrLn "*. Back"
            putStrLn "q. Quit"
            putStrLn "Enter option:"
            option <- getLine
            case option of
                "#" -> searchLoop
                "*" -> searchBySeverity
                "q" -> return ()
                _ -> do
                    putStrLn "Invalid option."
                    performSeveritySearch severity

displayResults :: [(T.Text, T.Text)] -> IO ()
displayResults results = do
    putStrLn "Select a road:"
    mapM_ (\(i, (rid, name)) -> printf "%d. %s (%s)\n" (i :: Int) name rid) (zip [1..] results)
    putStrLn "s. Search again"
    putStrLn "#. Back to Main Menu"
    putStrLn "*. Back"
    putStrLn "q. Quit"
    putStrLn "Enter number or option:"
    input <- getLine
    case input of
        "s" -> searchByName
        "#" -> searchLoop
        "*" -> searchByName
        "q" -> return ()
        _ -> do
            let index = readMaybe input :: Maybe Int
            case index of
                Just idx | idx > 0 && idx <= length results -> do
                    let (rid, name) = results !! (idx - 1)
                    putStrLn $ "\nSelected: " ++ T.unpack name
                    printRoadStatus rid
                    promptContinuation
                _ -> putStrLn "Invalid selection."

promptContinuation :: IO ()
promptContinuation = do
    putStrLn "\n"
    putStrLn "Do you want to check the status for any other road as well? or quit? (y/N)"
    choice <- getLine
    case choice of
        "y" -> searchLoop
        "Y" -> searchLoop
        _ -> return ()

printRoadStatus :: T.Text -> IO ()
printRoadStatus rid = do
    status <- getLatestStatus rid
    case status of
        [] -> putStrLn "No status data available."
        ((name, sev, desc, url, start, end, time):_) -> do
            putStrLn ""
            putStrLn $ bold "Name: " ++ T.unpack name
            putStrLn $ bold "Severity Status: " ++ colorizeSeverity sev (T.unpack sev)
            putStrLn $ bold "Severity Description: " ++ T.unpack desc
            case url of
                Just u -> putStrLn $ bold "URL: " ++ "https://tfl.gov.uk" ++ T.unpack u
                Nothing -> return ()
            case start of
                Just s -> putStrLn $ bold "Start Date: " ++ T.unpack s
                Nothing -> return ()
            case end of
                Just e -> putStrLn $ bold "End Date: " ++ T.unpack e
                Nothing -> return ()
            putStrLn $ bold "Last Updated: " ++ T.unpack time

-- | Helper to bold text using ANSI codes
bold :: String -> String
bold s = "\ESC[1m" ++ s ++ "\ESC[0m"

-- | Helper to colorize severity
colorizeSeverity :: T.Text -> String -> String
colorizeSeverity sevText s
    | sev == "Good" = "\ESC[32m" ++ s ++ "\ESC[0m" -- Green
    | sev == "Serious" || sev == "Severe" = "\ESC[31m" ++ s ++ "\ESC[0m" -- Red
    | otherwise = "\ESC[33m" ++ s ++ "\ESC[0m" -- Yellow (Warning/Other)
  where sev = T.unpack sevText
