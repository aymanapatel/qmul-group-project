{-# LANGUAGE OverloadedStrings #-}
module Actions.Search.ByCoordinates (
    searchByCoordinates
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (eitherDecode)
import Text.Printf (printf)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import qualified Database as DB
import Types (CoordinateEntry(..))
import Actions.Search.Common (displayResultsWithDetails, exitApp, printSeparator)
import Utils.Display (box)

searchByCoordinates :: IO () -> IO ()
searchByCoordinates mainMenu = do
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
        "1" -> searchByPredefinedList mainMenu
        "2" -> searchByManualCoordinates mainMenu
        "home" -> mainMenu
        "exit" -> exitApp
        _ -> do
            putStrLn "\nPlease select an option from the given list."
            searchByCoordinates mainMenu

searchByPredefinedList :: IO () -> IO ()
searchByPredefinedList mainMenu = do
    exists <- doesFileExist "coordinates.json"
    if not exists
        then do
            putStrLn "Configuration Error: 'coordinates.json' file is missing."
            searchByCoordinates mainMenu
        else do
            content <- LBS.readFile "coordinates.json"
            case (eitherDecode content :: Either String [CoordinateEntry]) of
                Left err -> do
                    putStrLn $ "Error parsing coordinates.json: " ++ err
                    searchByCoordinates mainMenu
                Right entries -> do
                    putStrLn "\nSelect a location:"
                    mapM_ (\(i, entry) -> printf "%d. %s (%s) - %s\n" (i :: Int) (T.unpack $ coordCorridorName entry) (T.unpack $ coordArea entry) (T.unpack $ coordPersonName entry)) (zip [1..] entries)
                    putStrLn "\nhome. Home Page\nb. Back\nexit. Exit Application\n"
                    printSeparator
                    putStrLn "\n\nEnter option:"
                    input <- getLine
                    case input of
                        "home" -> mainMenu
                        "b" -> searchByCoordinates mainMenu
                        "exit" -> exitApp
                        _ -> case readMaybe input :: Maybe Int of
                            Just idx | idx > 0 && idx <= length entries -> do
                                let entry = entries !! (idx - 1)
                                putStrLn $ "\nSelected: " ++ T.unpack (coordCorridorName entry)
                                performCoordinateSearch (coordLat entry) (coordLong entry) mainMenu
                            _ -> do
                                putStrLn "\nPlease select an option from the given list."
                                searchByPredefinedList mainMenu

searchByManualCoordinates :: IO () -> IO ()
searchByManualCoordinates mainMenu = do
    printSeparator
    putStrLn "\nPlease enter Longitude (e.g., -0.1278) or press 'b' to return:"
    lonStr <- getLine
    case lonStr of
        "b" -> searchByCoordinates mainMenu
        _ -> case readMaybe lonStr :: Maybe Double of
            Nothing -> do
                putStrLn "The longitude entered is invalid. Please try again."
                searchByManualCoordinates mainMenu
            Just lon -> do
                putStrLn "Enter Latitude (e.g., 51.5074):"
                latStr <- getLine
                case readMaybe latStr :: Maybe Double of
                    Nothing -> do
                        putStrLn "Invalid latitude."
                        searchByManualCoordinates mainMenu
                    Just lat -> performCoordinateSearch lat lon mainMenu

performCoordinateSearch :: Double -> Double -> IO () -> IO ()
performCoordinateSearch lat lon mainMenu = do
    putStrLn $ "\nLocating roads near " ++ show lat ++ ", " ++ show lon ++ "..."
    results <- DB.getNearestRoads lat lon 5
    if null results
        then putStrLn "No roads were found within the search radius."
        else displayResultsWithDetails results (searchByCoordinates mainMenu) mainMenu (searchByCoordinates mainMenu)
