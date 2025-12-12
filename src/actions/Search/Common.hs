{-# LANGUAGE OverloadedStrings #-}
module Actions.Search.Common (
    displayResults,
    printRoadStatus,
    promptContinuation,
    displayResultsWithDetails,
    findBestOption,
    findWorstOption,
    fetchRoadDetailedStatus,
    fetchRoadDetailedStatus,
    displayRoadDetailedStatus,
    exitApp,
    printSeparator
) where

import Database.SQLite.Simple (open, close, query, Only(..))
import qualified Data.Text as T
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Database as DB
import Types (Disruption(..), RoadDetailedDisplayData(..))
import Utils.Display (bold, colorizeSeverity, box)

-- | Displays a list of roads and allows selection.
-- Takes `searchAgain`, `mainMenu`, `back` as actions.
displayResults :: [(T.Text, T.Text)] -> IO () -> IO () -> IO () -> IO ()
displayResults results searchAgain mainMenu back = do
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
        "home" -> mainMenu
        "prev" -> back
        "exit" -> exitApp
        _ -> do
            let index = readMaybe input :: Maybe Int
            case index of
                Just idx | idx > 0 && idx <= length results -> do
                    let (rid, name) = results !! (idx - 1)
                    putStrLn $ "\nSelected: " ++ T.unpack name
                    printRoadStatus rid mainMenu
                    promptContinuation mainMenu
                _ -> putStrLn "Invalid selection. Please choose from the available options."

-- | Prompts user to check another road or quit.
-- Takes `mainMenu` action.
promptContinuation :: IO () -> IO ()
promptContinuation mainMenu = do
    putStrLn "\n"
    printSeparator
    putStrLn "Would you like to check another road? Type 'y' to continue or enter to go back to the home page."
    choice <- getLine
    case choice of
        "y" -> mainMenu
        "Y" -> mainMenu
        "" -> mainMenu
        _ -> mainMenu

-- | Fetches aggregated detailed status for a road.
-- Returns Maybe RoadDetailedDisplayData
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
printRoadStatus rid _ = do
    maybeData <- fetchRoadDetailedStatus rid
    case maybeData of
        Nothing -> putStrLn "Status information is currently unavailable."
        Just dataVal -> displayRoadDetailedStatus dataVal

-- | Displays results with distance details (for coordinate search).
displayResultsWithDetails :: [(T.Text, T.Text, Double, T.Text, T.Text)] -> IO () -> IO () -> IO () -> IO ()
displayResultsWithDetails results searchAgain mainMenu back = do
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
        "home" -> mainMenu
        "prev" -> back  
        "exit" -> exitApp
        _ -> do
            putStrLn "Invalid selection. Please choose from the available options."
            displayResultsWithDetails results searchAgain mainMenu back

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

-- | Prints exit message.
exitApp :: IO ()
exitApp = do
    putStrLn "\n********************************************************************************"
    putStrLn "*                                                                              *"
    putStrLn "*   Thanks for using the application. Happy to help you again.                 *"
    putStrLn "*                            !!Cheers!!                                        *"
    putStrLn "********************************************************************************"
printSeparator :: IO ()
printSeparator = putStrLn $ replicate 50 '-'
