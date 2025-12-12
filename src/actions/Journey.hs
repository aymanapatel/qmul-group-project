{-# LANGUAGE OverloadedStrings #-}
module Actions.Journey (
    planJourney
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (eitherDecode)
import Text.Printf (printf)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import qualified Database as DB
import Types (CoordinateEntry(..))
import Actions.Search.Common (printSeparator, exitApp)
import Utils.Display (box)
import System.Exit (exitSuccess)

-- | Main entry point for the Journey Planner feature.
-- Guides the user through a friendly wizard to select trip details.
planJourney :: IO ()
planJourney = do
    putStrLn "\n********************************************************************************"
    putStrLn "*                                                                              *"
    putStrLn "*                     My Journey Planner                                       *"
    putStrLn "*           Check for disruptions before you travel!                           *"
    putStrLn "*                                                                              *"
    putStrLn "********************************************************************************"
    putStrLn ""

    putStrLn "Step 1: Where are you starting from?"
    startPoint <- selectLocation
    case startPoint of
        Nothing -> cancelJourney
        Just startData -> do
            
            printSeparator
            putStrLn "Step 2: Where do you want to go?"
            endPoint <- selectLocation
            case endPoint of
                Nothing -> cancelJourney
                Just endData -> displayJourneyResults startData endData

    where
        cancelJourney = putStrLn "\nJourney planning cancelled. Returning to main menu..."

-- | Prompts user to select a location via Predefined List or Manual Input.
-- Returns: 'Just (Lat, Lon, DisplayName)' or 'Nothing' if cancelled.
selectLocation :: IO (Maybe (Double, Double, String))
selectLocation = do
    putStrLn "\nHow would you like to select this location?"
    let menuOptions = [ "1. Select from predefined list"
                      , "2. Enter custom coordinates"
                      , " "
                      , "\"exit\" to exit the application"
                      ]
    mapM_ putStrLn (box menuOptions)
    printSeparator
    putStrLn "\n\nEnter option:"
    option <- getLine
    case option of
        "1" -> selectPredefinedLocation
        "2" -> selectManualLocation
        "exit" -> do
            exitApp
            exitSuccess
        _ -> do
             putStrLn "Invalid option, please try again."
             selectLocation

-- | Displays the predefined list of locations for selection.
selectPredefinedLocation :: IO (Maybe (Double, Double, String))
selectPredefinedLocation = do
    exists <- doesFileExist "coordinates.json"
    if not exists
        then do
            putStrLn "Error: Configuration file 'coordinates.json' is missing."
            return Nothing
        else do
            content <- LBS.readFile "coordinates.json"
            case (eitherDecode content :: Either String [CoordinateEntry]) of
                Left err -> do
                    putStrLn $ "Error parsing coordinates.json: " ++ err
                    return Nothing
                Right entries -> do
                    putStrLn "\nAvailable Locations:"
                    mapM_ (\(i, entry) -> printf "%d. %s (%s) - %s\n" 
                        (i :: Int) 
                        (T.unpack $ coordCorridorName entry) 
                        (T.unpack $ coordArea entry) 
                        (T.unpack $ coordPersonName entry)) (zip [1..] entries)
                    
                    printSeparator
                    putStrLn "Enter number to select, or 'b' to go back:"
                    input <- getLine
                    
                    case input of
                        "exit" -> exitApp >> exitSuccess
                        "b" -> selectLocation
                        _ -> case readMaybe input :: Maybe Int of
                            Just idx | idx > 0 && idx <= length entries -> do
                                let entry = entries !! (idx - 1)
                                let name = T.unpack (coordCorridorName entry) ++ " (" ++ T.unpack (coordArea entry) ++ ")"
                                return $ Just (coordLat entry, coordLong entry, name)
                            _ -> do
                                putStrLn "Invalid selection. Please try again."
                                selectPredefinedLocation

-- | Allows manual entry of Latitude and Longitude.
selectManualLocation :: IO (Maybe (Double, Double, String))
selectManualLocation = do
    putStrLn "\n[Manual Entry]"
    putStrLn "Please enter the Longitude (e.g., -0.1278) or 'b' to go back:"
    lonStr <- getLine
    if lonStr == "b" then selectLocation else
        case lonStr of
            "exit" -> exitApp >> exitSuccess
            _ -> case readMaybe lonStr :: Maybe Double of
                Nothing -> putStrLn "That doesn't look like a valid longitude. Please try again." >> selectManualLocation
                Just lon -> do
                    putStrLn "Please enter the Latitude (e.g., 51.5074):"
                    latStr <- getLine
                    case latStr of
                        "exit" -> exitApp >> exitSuccess
                        _ -> case readMaybe latStr :: Maybe Double of
                            Nothing -> putStrLn "That doesn't look like a valid latitude. Please try again." >> selectManualLocation
                            Just lat -> return $ Just (lat, lon, "Custom Coordinates")

-- | Displays the journey analysis results.
displayJourneyResults :: (Double, Double, String) -> (Double, Double, String) -> IO ()
displayJourneyResults (lat1, lon1, name1) (lat2, lon2, name2) = do
    printSeparator
    putStrLn $ "\n🚉  JOURNEY REPORT"
    printf "Start: %s (Lat: %.4f, Lon: %.4f)\n" name1 lat1 lon1
    printf "End:   %s (Lat: %.4f, Lon: %.4f)\n" name2 lat2 lon2
    printSeparator
    
    putStrLn $ "\n--- Checking Start Point ---"
    startRoads <- checkPoint lat1 lon1
    
    putStrLn $ "\n--- Checking Destination ---"
    endRoads <- checkPoint lat2 lon2
    
    printSeparator
    putStrLn "\n--- Route Recommendation ---"
    analyzeJourney startRoads endRoads
    
    printSeparator
    putStrLn "Analysis complete. Have a safe and pleasant trip!"
    exitApp

-- | Fetches and prints road status near a coordinate. Returns data for analysis.
checkPoint :: Double -> Double -> IO [(T.Text, T.Text)]
checkPoint lat lon = do
    results <- DB.getNearestRoads lat lon 5
    if null results
        then do
            putStrLn "We couldn't locate any major roads nearby."
            return []
        else do
            -- Print details for user
            mapM_ (\(i, (_, name, dist, sev, desc)) -> 
                printf "- %s (%.2f miles): %s - %s\n" 
                    (T.unpack name) dist (T.unpack sev) (T.unpack desc)
             ) (zip [(1::Int)..] results)
            
            -- Return simple list of (Name, Severity) for logic
            return $ map (\(_, name, _, sev, _) -> (name, sev)) results

-- | Analyzes road statuses to provide a friendly recommendation.
analyzeJourney :: [(T.Text, T.Text)] -> [(T.Text, T.Text)] -> IO ()
analyzeJourney startRoads endRoads = do
    let startStatus = getBestStatus startRoads
    let endStatus   = getBestStatus endRoads
    
    let message = case (startStatus, endStatus) of
            (Nothing, _) -> 
                [ "⚠️  We couldn't find enough road data for your start point to make a recommendation."
                , "Please verify your location coordinates."
                ]
            (_, Nothing) -> 
                [ "⚠️  We couldn't find enough road data for your destination to make a recommendation."
                , "Please verify your location coordinates."
                ]
            (Just "Good", Just "Good") ->
                [ "✅  GOOD NEWS!"
                , "Both your start point and destination appear clear of major disruptions."
                , "It looks like a great time to travel!"
                ]
            (Just sSev, Just eSev) ->
                [ "⚠️  HEADS UP:"
                , if sSev /= "Good" then printf "   - Reports of %s service at your start point." (T.unpack sSev) else ""
                , if eSev /= "Good" then printf "   - Reports of %s service at your destination." (T.unpack eSev) else ""
                , " "
                , "You might want to consider an alternative route or allow extra travel time."
                ]
    
    -- Filter out empty strings from formatting logic above
    mapM_ putStrLn (filter (not . null) message)

    where
        -- | Finds the 'best' status in a list of roads.
        -- If any road is Good, returns Good. Otherwise returns the first one found.
        getBestStatus :: [(T.Text, T.Text)] -> Maybe T.Text
        getBestStatus [] = Nothing
        getBestStatus roads = 
            let severities = map snd roads
            in if "Good" `elem` severities 
               then Just "Good" 
               else Just (head severities)
