{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (encode, eitherDecode)
import Types
import Fetch
import Parse
import Database
import Text.Printf (printf)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)

import Database.SQLite.Simple
import qualified Data.Text as T
import Utils.Env
import Utils.Display

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
                    roadResult <- downloadRoads apiKey
                    case roadResult of
                        Left err -> putStrLn err
                        Right jsonRoads -> do
                            case parseRoads jsonRoads of
                                Left err -> putStrLn $ "Error parsing roads: " ++ err
                                Right roads -> do
                                    putStrLn "Processing road geometry..."
                                    let processedRoads = processRoads roads
                                    putStrLn $ "Saving " ++ show (length processedRoads) ++ " roads to database..."
                                    saveRoads processedRoads
                                    
                                    putStrLn "Downloading disruption data..."
                                    disruptionResult <- downloadDisruptions apiKey
                                    case disruptionResult of
                                        Left err -> putStrLn err
                                        Right jsonDisruptions -> do
                                            case parseDisruptions jsonDisruptions of
                                                Left err -> putStrLn $ "Error parsing disruptions: " ++ err
                                                Right disruptions -> do
                                                    putStrLn "Processing disruptions and finding nearest roads..."
                                                    let processedDisruptions = processDisruptions disruptions processedRoads
                                                    putStrLn $ "Saving " ++ show (length processedDisruptions) ++ " disruptions to database..."
                                                    saveDisruptions processedDisruptions
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
            printf ("%-25s | %-10s | %-10s | %-10s\n" :: String) ("Road Name" :: String) ("Total Logs" :: String) ("Good Svc" :: String) ("Reliability" :: String)
            putStrLn "----------------------------------------------------------------"
            mapM_ (\(name, total, good, reliability) -> 
                printf ("%-25s | %-10d | %-10d | %-9.1f%%\n" :: String) (T.unpack name) total good reliability) report
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
    putStrLn "3. Search by coordinates"
    putStrLn "q. Quit"
    putStrLn "Enter option:"
    option <- getLine
    case option of
        "1" -> searchByName
        "2" -> searchBySeverity
        "3" -> searchByCoordinates
        "q" -> return ()
        _ -> do
            putStrLn "Invalid option."
            searchLoop

searchByCoordinates :: IO ()
searchByCoordinates = do
    putStrLn "\n--- Search by Coordinates Menu ---"
    putStrLn "1. Select from Predefined List"
    putStrLn "2. Enter Coordinates Manually"
    putStrLn "#. Back to Main Menu"
    putStrLn "q. Quit"
    putStrLn "Enter option:"
    option <- getLine
    case option of
        "1" -> searchByPredefinedList
        "2" -> searchByManualCoordinates
        "#" -> searchLoop
        "q" -> return ()
        _ -> do
            putStrLn "Invalid option."
            searchByCoordinates

searchByPredefinedList :: IO ()
searchByPredefinedList = do
    exists <- doesFileExist "coordinates.json"
    if not exists
        then do
            putStrLn "Error: coordinates.json not found."
            searchByCoordinates
        else do
            content <- LBS.readFile "coordinates.json"
            case (eitherDecode content :: Either String [CoordinateEntry]) of
                Left err -> do
                    putStrLn $ "Error parsing coordinates.json: " ++ err
                    searchByCoordinates
                Right entries -> do
                    putStrLn "\nSelect a location:"
                    mapM_ (\(i, entry) -> printf "%d. %s (%s) - %s\n" (i :: Int) (T.unpack $ coordCorridorName entry) (T.unpack $ coordArea entry) (T.unpack $ coordPersonName entry)) (zip [1..] entries)
                    putStrLn "#. Back"
                    putStrLn "q. Quit"
                    putStrLn "Enter number:"
                    input <- getLine
                    case input of
                        "#" -> searchByCoordinates
                        "q" -> return ()
                        _ -> case readMaybe input :: Maybe Int of
                            Just idx | idx > 0 && idx <= length entries -> do
                                let entry = entries !! (idx - 1)
                                putStrLn $ "\nSelected: " ++ T.unpack (coordCorridorName entry)
                                performCoordinateSearch (coordLat entry) (coordLong entry)
                            _ -> do
                                putStrLn "Invalid selection."
                                searchByPredefinedList

searchByManualCoordinates :: IO ()
searchByManualCoordinates = do
    putStrLn "\nEnter Longitude (e.g., -0.1278) or '#' to go back:"
    lonStr <- getLine
    case lonStr of
        "#" -> searchByCoordinates
        _ -> case readMaybe lonStr :: Maybe Double of
            Nothing -> do
                putStrLn "Invalid longitude."
                searchByManualCoordinates
            Just lon -> do
                putStrLn "Enter Latitude (e.g., 51.5074):"
                latStr <- getLine
                case readMaybe latStr :: Maybe Double of
                    Nothing -> do
                        putStrLn "Invalid latitude."
                        searchByManualCoordinates
                    Just lat -> performCoordinateSearch lat lon

performCoordinateSearch :: Double -> Double -> IO ()
performCoordinateSearch lat lon = do
    putStrLn $ "\nSearching for roads near " ++ show lat ++ ", " ++ show lon ++ "..."
    results <- getNearestRoads lat lon 5
    if null results
        then putStrLn "No roads found nearby."
        else displayResultsWithDetails results searchByCoordinates searchByCoordinates

displayResultsWithDetails :: [(T.Text, T.Text, Double, T.Text, T.Text)] -> IO () -> IO () -> IO ()
displayResultsWithDetails results searchAgain back = do
    putStrLn "\nNearest Roads:"
    printf "%-4s %-25s %-15s %-15s %-30s\n" ("No." :: String) ("Name" :: String) ("Distance" :: String) ("Severity" :: String) ("Status" :: String)
    putStrLn $ replicate 95 '-'
    mapM_ (\(i, (_, name, dist, sev, desc)) -> 
        printf "%-4d %-25s %-15s %-15s %-30s\n" 
            (i :: Int) 
            (take 25 $ T.unpack name) 
            (printf "%.2f miles" dist :: String)
            (colorizeSeverity sev (take 15 $ T.unpack sev))
            (take 30 $ T.unpack desc)) (zip [1..] results)
        
    -- Recommendation Logic
    let bestOption = findBestOption results
    let worstOption = findWorstOption results
    
    putStrLn "\n--- Recommendations ---"
    case bestOption of
        Just (name, _, _, _) -> putStrLn $ "Best Option: " ++ T.unpack name ++ " (Closest road with Good Service)"
        Nothing -> putStrLn "Best Option: None (No nearby roads with Good Service)"
        
    case worstOption of
        Just (name, _, sev, _) -> putStrLn $ "Worst Option: " ++ T.unpack name ++ " (Avoid - " ++ T.unpack sev ++ " Service)"
        Nothing -> return ()

    putStrLn "-----------------------"
    
    -- Prompt for selection (using standard display logic mapping)
    let displayable = map (\(rid, name, _, _, _) -> (rid, name)) results
    displayResults displayable searchAgain back

findBestOption :: [(T.Text, T.Text, Double, T.Text, T.Text)] -> Maybe (T.Text, Double, T.Text, T.Text)
findBestOption results = 
    let goodRoads = filter (\(_, _, _, sev, _) -> sev == "Good") results
    in case goodRoads of
        [] -> Nothing
        ((_, name, dist, sev, desc):_) -> Just (name, dist, sev, desc) -- Already sorted by distance

findWorstOption :: [(T.Text, T.Text, Double, T.Text, T.Text)] -> Maybe (T.Text, Double, T.Text, T.Text)
findWorstOption results = 
    let badRoads = filter (\(_, _, _, sev, _) -> sev /= "Good") results
    in case badRoads of
        [] -> Nothing
        ((_, name, dist, sev, desc):_) -> Just (name, dist, sev, desc) -- Closest bad road

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
                    displayResults allRoads searchByName searchByName
                
                [(rid, name)] -> do
                    putStrLn $ "\nFound one road: " ++ T.unpack name
                    printRoadStatus rid
                    promptContinuation
                    
                _ -> displayResults results searchByName searchByName

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
            printf ("%-5s %-30s %-25s %-30s\n" :: String) ("No." :: String) ("Road Name" :: String) ("Timing" :: String) ("Description" :: String)
            putStrLn $ replicate 90 '-'
            mapM_ (\(i, (_, name, desc, time)) -> printf ("%-5d %-30s %-25s %-30s\n" :: String) (i :: Int) (T.unpack name) (T.unpack time) (T.unpack desc)) (zip [1..] results)
            
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

displayResults :: [(T.Text, T.Text)] -> IO () -> IO () -> IO ()
displayResults results searchAgain back = do
    putStrLn "Select a road:"
    mapM_ (\(i, (rid, name)) -> printf "%d. %s (%s)\n" (i :: Int) name rid) (zip [1..] results)
    putStrLn "s. Search again"
    putStrLn "#. Back to Main Menu"
    putStrLn "*. Back"
    putStrLn "q. Quit"
    putStrLn "Enter number or option:"
    input <- getLine
    case input of
        "s" -> searchAgain
        "#" -> searchLoop
        "*" -> back
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
            
            -- Show nearby disruptions
            disruptions <- getDisruptionsForRoad rid
            if not (null disruptions)
                then do
                    putStrLn $ bold "\nNearby Disruptions:"
                    mapM_ (\d -> putStrLn $ "- " ++ T.unpack (disruptionDescription d)) disruptions
                else return ()
                
            -- Suggest alternative routes if severity is bad
            if sev /= "Good"
                then do
                    -- We need lat/lon of current road to find nearest good ones.
                    -- For now, we'll fetch the road again to get its coords (inefficient but simple)
                    -- Or we can assume we have them. Let's fetch from DB.
                    conn <- open "tfl.db"
                    [(_, _, lat, lon)] <- query conn "SELECT id, displayName, lat, lon FROM roads WHERE id = ?" (Only rid) :: IO [(T.Text, T.Text, Maybe Double, Maybe Double)]
                    close conn
                    
                    case (lat, lon) of
                        (Just rLat, Just rLon) -> do
                            goodRoads <- getNearestGoodRoads rLat rLon 3
                            if not (null goodRoads)
                                then do
                                    putStrLn $ bold "\nSuggested Alternative Routes (Good Status):"
                                    mapM_ (\(_, n, _) -> putStrLn $ "- " ++ T.unpack n) goodRoads
                                else putStrLn "\nNo nearby alternative routes found."
                        _ -> return ()
                else return ()


