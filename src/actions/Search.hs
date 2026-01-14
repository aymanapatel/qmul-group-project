{-# LANGUAGE OverloadedStrings #-}
module Actions.Search (
    searchLoop,
    -- * Exported for Journey
    displayResults,
    printRoadStatus,
    promptContinuation,
    displayResultsWithDetails,
    exitApp
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (eitherDecode)
import Text.Printf (printf)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)
import Database.SQLite.Simple (open, close, query, Only(..))

import qualified Database as DB
import Types (Disruption(..), RoadDetailedDisplayData(..), CoordinateEntry(..))
import Utils.Display (bold, colorizeSeverity, box, printSeparator)

-- | Main entry point for the search module.
searchLoop :: IO ()
searchLoop = do
    putStrLn "\n********************************************************************************"
    putStrLn "*                                                                              *"
    putStrLn "*   Welcome aboard, and thank you for using the TfL Disruptions Search System  *"
    putStrLn "*                     We're glad to have you here.                             *"
    putStrLn "*                                                                              *"
    putStrLn "*           Use our system to check the latest TfL disruptions.                *"
    putStrLn "*                                                                              *"
    putStrLn "*                       *** By Group 27 ***                                    *"
    putStrLn "*                                                                              *"
    putStrLn "********************************************************************************"
    runSearchMenu

-- | Displays the search menu and handles user input.
runSearchMenu :: IO ()
runSearchMenu = do
    putStrLn "\n"
    let menuOptions = [ ""
                        , "Search Menu"
                        , ""
                        , "1. Search by road name"
                        , "2. Search by severity level"
                        , "3. Search by coordinates"
                        , ""
                        , "\"exit\" to exit the application"    
                      ]
    mapM_ putStrLn (box menuOptions)
    putStrLn "\n\nEnter option:"
    option <- getLine
    case option of
        "1" -> searchByName >> runSearchMenu
        "2" -> searchBySeverity >> runSearchMenu
        "3" -> searchByCoordinates >> runSearchMenu
        "exit" -> exitApp
        _ -> do
            putStrLn "\nPlease select an option from the given list."
            runSearchMenu

-- * Search By Name

searchByName :: IO ()
searchByName = do
    printSeparator
    putStrLn "\nPlease enter a road name or enter 'prev' to return to the previous menu:"
    queryStr <- getLine
    case queryStr of
        "prev" -> return ()
        _ -> do
            -- 1. Try exact/substring search
            results <- DB.searchRoads (T.pack queryStr)
            case results of
                [] -> do
                     -- 2. Fallback: Search by first letter if query is not empty
                     if not (null queryStr)
                        then do
                            let firstChar = head queryStr
                            putStrLn $ "\nNo roads found matching \"" ++ queryStr ++ "\"."
                            putStrLn $ "\n\nWe couldn't find an exact match. Searching by the first letter instead...\nSearching for roads matching '" ++ [firstChar] ++ "'..."
                            
                            fallbackResults <- DB.searchRoadsByFirstChar (T.pack [firstChar])
                            case fallbackResults of
                                [] -> do
                                    -- 3. Fallback: Show all roads
                                    putStrLn $ "\nNo roads found starting with '" ++ [firstChar] ++ "'. Showing all available roads:\n"
                                    allRoads <- DB.getAllRoads
                                    displayResults allRoads searchByName (return ()) (return ()) -- home and back just return to menu
                                
                                _ -> displayResults fallbackResults searchByName (return ()) (return ())
                        else do
                            putStrLn "\nNo input provided. Showing all available roads:"
                            allRoads <- DB.getAllRoads
                            displayResults allRoads searchByName (return ()) (return ())

                [(rid, name)] -> do
                    putStrLn $ "\nSingle match found: " ++ T.unpack name
                    printRoadStatus rid (return ())
                    promptContinuation (return ())
                    searchByName -- Loop back to search again? Or return? Original logic was murky. 
                    -- promptContinuation calls the passed action if "y". If passed "return ()", it just returns.
                    -- If we want to stay in searchByName loop, we should recursively call it.
                    
                _ -> displayResults results searchByName (return ()) (return ())

-- * Search By Severity

searchBySeverity :: IO ()
searchBySeverity = do
    let menuOptions = [ ""
                      , "Search by Severity"
                      , ""
                      , "1. Good"
                      , "2. Serious"
                      , "3. Severe"
                      , ""
                      , "\"prev\" to go to the previous page"
                      , "\"home\" to return to the home page"
                      , "\"exit\" to exit the application"
                      ]
    mapM_ putStrLn (box menuOptions)
    putStrLn "\n\nEnter option:"
    option <- getLine
    case option of
        "1" -> performSeveritySearch "Good" >> searchBySeverity
        "2" -> performSeveritySearch "Serious" >> searchBySeverity
        "3" -> performSeveritySearch "Severe" >> searchBySeverity
        "prev" -> return ()
        "home" -> return ()
        "exit" -> exitApp
        _ -> do
            putStrLn "\nPlease select an option from the given list."
            searchBySeverity

performSeveritySearch :: String -> IO ()
performSeveritySearch severity = do
    results <- DB.getRoadsBySeverity (T.pack severity)
    if null results
        then putStrLn $ "\nNo roads currently match the severity status: " ++ severity ++ "\n"
        else do
            putStrLn $ "\nRoads reporting status: " ++ severity ++ "\n"
            printf ("%-5s %-30s %-25s %-30s\n" :: String) ("No." :: String) ("Road Name" :: String) ("Timing" :: String) ("Description" :: String)
            putStrLn $ replicate 90 '-'
            mapM_ (\(i, (_, name, desc, time)) -> printf ("%-5d %-30s %-25s %-30s\n" :: String) (i :: Int) (T.unpack name) (T.unpack time) (T.unpack desc)) (zip [1..] results)
            
            -- Simple wait/prompt
            putStrLn "\nPress Enter to continue, 'home' for main menu, 'exit' to quit."
            opt <- getLine
            case opt of
                "exit" -> exitApp
                "home" -> return () -- Returning goes back to searchBySeverity loop which handles the menu
                _ -> return ()

-- * Search By Coordinates

searchByCoordinates :: IO ()
searchByCoordinates = do
    let menuOptions = [ ""
                      , "Search by Location"
                      , ""
                      , "1. Select a location from the predefined list"
                      , "2. Enter custom coordinates"
                      , ""
                      , "\"home\" to return to the home page"
                      , "\"exit\" to exit the application"
                      , ""
                      ]
    mapM_ putStrLn (box menuOptions)
    printSeparator
    putStrLn "\n\nEnter option:"
    option <- getLine
    case option of
        "1" -> searchByPredefinedList >> searchByCoordinates
        "2" -> searchByManualCoordinates >> searchByCoordinates
        "home" -> return ()
        "exit" -> exitApp
        _ -> do
            putStrLn "\nPlease select an option from the given list."
            searchByCoordinates

searchByPredefinedList :: IO ()
searchByPredefinedList = do
    exists <- doesFileExist "coordinates.json"
    if not exists
        then putStrLn "Configuration Error: 'coordinates.json' file is missing."
        else do
            content <- LBS.readFile "coordinates.json"
            case (eitherDecode content :: Either String [CoordinateEntry]) of
                Left err -> putStrLn $ "Error parsing coordinates.json: " ++ err
                Right entries -> do
                    putStrLn "\nSelect a location:"
                    mapM_ (\(i, entry) -> printf "%d. %s (%s) - %s\n" (i :: Int) (T.unpack $ coordCorridorName entry) (T.unpack $ coordArea entry) (T.unpack $ coordPersonName entry)) (zip [1..] entries)
                    putStrLn "\n\"prev\" to go to the previous menu\n\"home\" to return to the home page\n\"exit\" to exit the application\n"
                    printSeparator
                    putStrLn "\n\nEnter option:"
                    input <- getLine
                    case input of
                        "home" -> return () -- Returning to searchByCoordinates which loops
                        "prev" -> return ()
                        "exit" -> exitApp
                        _ -> case readMaybe input :: Maybe Int of
                            Just idx | idx > 0 && idx <= length entries -> do
                                let entry = entries !! (idx - 1)
                                putStrLn $ "\nSelected: " ++ T.unpack (coordCorridorName entry)
                                performCoordinateSearch (coordLat entry) (coordLong entry)
                            _ -> do
                                putStrLn "\nPlease select an option from the given list."
                                searchByPredefinedList

searchByManualCoordinates :: IO ()
searchByManualCoordinates = do
    printSeparator
    putStrLn "\nPlease enter Longitude (e.g., -0.1278) or enter 'prev' to return to the previous menu:"
    lonStr <- getLine
    case lonStr of
        "prev" -> return ()
        _ -> case readMaybe lonStr :: Maybe Double of
            Nothing -> do
                putStrLn "The longitude entered is invalid. Please try again."
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
    putStrLn $ "\nLocating roads near " ++ show lat ++ ", " ++ show lon ++ "..."
    results <- DB.getNearestRoads lat lon 5
    if null results
        then putStrLn "No roads were found within the search radius."
        else displayResultsWithDetails results (performCoordinateSearch lat lon) (return ()) (return ())

-- * Common Display Functions

-- | Displays a list of roads and allows selection.
-- Takes `searchAgain`, `mainMenu`, `back` as actions.
-- Reformatted to just take IO actions for simple navigation handling.
displayResults :: [(T.Text, T.Text)] -> IO () -> IO () -> IO () -> IO ()
displayResults results searchAgain home back = do
    putStrLn "\nPlease select a road to view its details:"
    mapM_ (\(i, (rid, name)) -> printf "%d. %s (%s)\n" (i :: Int) name rid) (zip [1..] results)
    putStrLn "\n s. Search again"
    putStrLn "\"prev\" to go to the previous menu"
    putStrLn "\"home\" to return to the home page"
    putStrLn "\"exit\" to exit the application"
    printSeparator
    putStrLn "\n\nEnter option:"
    input <- getLine
    case input of
        "s" -> searchAgain
        "home" -> home
        "prev" -> back
        "exit" -> exitApp
        _ -> do
            let index = readMaybe input :: Maybe Int
            case index of
                Just idx | idx > 0 && idx <= length results -> do
                    let (rid, name) = results !! (idx - 1)
                    putStrLn $ "\nSelected: " ++ T.unpack name
                    printRoadStatus rid home
                    promptContinuation home
                _ -> do
                    putStrLn "Invalid selection. Please choose from the available options."
                    displayResults results searchAgain home back

-- | Prompts user to check another road or quit.
promptContinuation :: IO () -> IO ()
promptContinuation home = do
    putStrLn "\n"
    printSeparator
    putStrLn "Would you like to check another road? Type 'y' to continue or enter to go back to the home page."
    choice <- getLine
    case choice of
        "y" -> return () -- Assuming continue means "do nothing, return to caller" or "go to menu"? 
                         -- In updated logic, "return ()" usually goes back up the stack.
        "Y" -> return ()
        "" -> home       -- Enter -> Home
        _ -> home

-- | Fetches aggregated detailed status for a road.
fetchRoadDetailedStatus :: T.Text -> IO (Maybe RoadDetailedDisplayData)
fetchRoadDetailedStatus rid = do
    status <- DB.getLatestStatus rid
    case status of
        [] -> return Nothing
        ((name, sev, desc, url, start, end, time):_) -> do
            -- Fetch disruptions
            disruptions <- DB.getDisruptionsForRoad rid
            
            -- Fetch alternatives if severity is bad
            alternatives <- if sev /= "Good"
                then do
                    conn <- open "tfl.db"
                    -- We need lat/lon of current road to find nearest good ones.
                    roadCoords <- query conn "SELECT id, displayName, lat, lon FROM roads WHERE id = ?" (Only rid) :: IO [(T.Text, T.Text, Maybe Double, Maybe Double)]
                    close conn
                    
                    case roadCoords of
                        [(_, _, Just rLat, Just rLon)] -> DB.getNearestGoodRoads rLat rLon 3
                        _ -> return []
                else return []
            
            return $ Just $ RoadDetailedDisplayData
                { rddName = name
                , rddSeverity = sev
                , rddDescription = desc
                , rddUrl = url
                , rddStartDate = start
                , rddEndDate = end
                , rddLastUpdated = time
                , rddDisruptions = disruptions
                , rddAlternativeRoutes = alternatives
                }

-- | Displays the aggregated road details.
displayRoadDetailedStatus :: RoadDetailedDisplayData -> IO ()
displayRoadDetailedStatus dataVal = do
    putStrLn ""
    putStrLn $ bold "Name: " ++ T.unpack (rddName dataVal)
    putStrLn $ bold "Severity Status: " ++ colorizeSeverity (rddSeverity dataVal) (T.unpack $ rddSeverity dataVal)
    putStrLn $ bold "Severity Description: " ++ T.unpack (rddDescription dataVal)
    
    case rddUrl dataVal of
        Just u -> putStrLn $ bold "URL: " ++ "https://tfl.gov.uk" ++ T.unpack u
        Nothing -> return ()
        
    case rddStartDate dataVal of
        Just s -> putStrLn $ bold "Start Date: " ++ T.unpack s
        Nothing -> return ()
        
    case rddEndDate dataVal of
        Just e -> putStrLn $ bold "End Date: " ++ T.unpack e
        Nothing -> return ()
        
    putStrLn $ bold "Last Updated: " ++ T.unpack (rddLastUpdated dataVal)
    
    -- Show nearby disruptions
    let disruptions = rddDisruptions dataVal
    if not (null disruptions)
        then do
            putStrLn $ bold "\nNearby Disruptions:"
            mapM_ (\d -> putStrLn $ "- " ++ T.unpack (disruptionDescription d)) disruptions
        else return ()
        
    -- Show alternatives
    let alternatives = rddAlternativeRoutes dataVal
    if not (null alternatives)
        then do
            putStrLn $ bold "\nSuggested Alternative Routes (Good Status):"
            mapM_ (\(_, n, _) -> putStrLn $ "- " ++ T.unpack n) alternatives
    else if rddSeverity dataVal /= "Good" 
        then putStrLn "\nNo nearby alternative routes found."
        else return ()

-- | Prints detailed status for a road using fetch and display functions.
printRoadStatus :: T.Text -> IO () -> IO ()
printRoadStatus rid home = do
    maybeData <- fetchRoadDetailedStatus rid
    case maybeData of
        Nothing -> putStrLn "Status information is currently unavailable."
        Just dataVal -> displayRoadDetailedStatus dataVal

-- | Displays results with distance details (for coordinate search).
displayResultsWithDetails :: [(T.Text, T.Text, Double, T.Text, T.Text)] -> IO () -> IO () -> IO () -> IO ()
displayResultsWithDetails results searchAgain home back = do
    putStrLn "\nClosest Roads Found:"
    printf "%-4s %-25s %-15s %-15s %-30s\n" ("No." :: String) ("Name" :: String) ("Distance" :: String) ("Severity" :: String) ("Status" :: String)
    putStrLn $ replicate 80 '-'
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
        Just (name, _, _, _) -> putStrLn $ "Recommended Route: " ++ T.unpack name ++ " (Closest road with Good Service)"
        Nothing -> putStrLn "Recommended Route: None (No nearby roads with Good Service)"
        
    case worstOption of
        Just (name, _, sev, _) -> putStrLn $ "Route to Avoid: " ++ T.unpack name ++ " (Avoid - " ++ T.unpack sev ++ " Service)"
        Nothing -> return ()

    putStrLn "-----------------------"

    -- Simple Navigation Menu (No Selection)
    let menuOptions = [ "s. Search again"
                      , "\"prev\" to go to the previous page"
                      , "\"home\" to return to the home page"
                      , "\"exit\" to exit the application"
                      ]
    mapM_ putStrLn (box menuOptions)
    printSeparator
    putStrLn "Enter option:"
    
    input <- getLine
    case input of
        "s" -> searchAgain
        "home" -> home
        "prev" -> back  
        "exit" -> exitApp
        _ -> do
            putStrLn "Invalid selection. Please choose from the available options."
            displayResultsWithDetails results searchAgain home back

findBestOption :: [(T.Text, T.Text, Double, T.Text, T.Text)] -> Maybe (T.Text, Double, T.Text, T.Text)
findBestOption results = 
    let goodRoads = filter (\(_, _, _, sev, _) -> sev == "Good") results
    in case goodRoads of
        [] -> Nothing
        ((_, name, dist, sev, desc):_) -> Just (name, dist, sev, desc)

findWorstOption :: [(T.Text, T.Text, Double, T.Text, T.Text)] -> Maybe (T.Text, Double, T.Text, T.Text)
findWorstOption results = 
    let badRoads = filter (\(_, _, _, sev, _) -> sev /= "Good") results
    in case badRoads of
        [] -> Nothing
        ((_, name, dist, sev, desc):_) -> Just (name, dist, sev, desc)

-- | Prints exit message and exits.
exitApp :: IO ()
exitApp = do
    putStrLn "\n********************************************************************************"
    putStrLn "*                                                                              *"
    putStrLn "*   Thanks for using the application. Happy to help you again.                 *"
    putStrLn "*                            !!Cheers!!                                        *"
    putStrLn "********************************************************************************"
    exitSuccess
