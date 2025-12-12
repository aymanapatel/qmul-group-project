{-# LANGUAGE OverloadedStrings #-}
module Actions.Search.BySeverity (
    searchBySeverity
) where

import qualified Data.Text as T
import Text.Printf (printf)
import qualified Database as DB
import Actions.Search.Common (exitApp, printSeparator)
import Utils.Display (box)

searchBySeverity :: IO () -> IO ()
searchBySeverity mainMenu = do
    let menuOptions = [ ""
                      , "Search by Severity"
                      , ""
                      , "1. Good"
                      , "2. Serious"
                      , "3. Severe"
                      , ""
                      , "b. Back"
                      , "\"home\" to return to the home page"
                      , "\"exit\" to exit the application"
                      ]
    mapM_ putStrLn (box menuOptions)
    putStrLn "\n\nEnter option:"
    option <- getLine
    case option of
        "1" -> performSeveritySearch "Good" mainMenu
        "2" -> performSeveritySearch "Serious" mainMenu
        "3" -> performSeveritySearch "Severe" mainMenu
        "b" -> mainMenu
        "home" -> mainMenu
        "exit" -> exitApp
        _ -> do
            putStrLn "\nPlease select an option from the given list."
            searchBySeverity mainMenu

performSeveritySearch :: String -> IO () -> IO ()
performSeveritySearch severity mainMenu = do
    results <- DB.getRoadsBySeverity (T.pack severity)
    if null results
        then do
            putStrLn $ "\nNo roads currently match the severity status: " ++ severity ++ "\n"
            searchBySeverity mainMenu
        else do
            putStrLn $ "\nRoads reporting status: " ++ severity ++ "\n"
            printf ("%-5s %-30s %-25s %-30s\n" :: String) ("No." :: String) ("Road Name" :: String) ("Timing" :: String) ("Description" :: String)
            putStrLn $ replicate 90 '-'
            mapM_ (\(i, (_, name, desc, time)) -> printf ("%-5d %-30s %-25s %-30s\n" :: String) (i :: Int) (T.unpack name) (T.unpack time) (T.unpack desc)) (zip [1..] results)
            putStrLn "\nb. Back\n\"home\" to return to the home page\n\"exit\" to exit the application\n"
            printSeparator
            putStrLn "Enter option:"
            option <- getLine
            case option of
                "home" -> mainMenu
                "b" -> searchBySeverity mainMenu
                "exit" -> exitApp
                _ -> do
                    putStrLn "\nPlease select an option from the given list."
                    performSeveritySearch severity mainMenu
