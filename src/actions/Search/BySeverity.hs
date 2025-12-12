{-# LANGUAGE OverloadedStrings #-}
module Actions.Search.BySeverity (
    searchBySeverity
) where

import qualified Data.Text as T
import Text.Printf (printf)
import qualified Database as DB
import Actions.Search.Common () -- no specific imports needed here, but kept for consistency if needed later

searchBySeverity :: IO () -> IO ()
searchBySeverity mainMenu = do
    putStrLn "\n--- Severity Search Menu ---"
    putStrLn "1. Good"
    putStrLn "2. Serious"
    putStrLn "3. Severe"
    putStrLn "\nb. Back"
    putStrLn "q. Quit"
    putStrLn "\n\nEnter option:"
    option <- getLine
    case option of
        "1" -> performSeveritySearch "Good" mainMenu
        "2" -> performSeveritySearch "Serious" mainMenu
        "3" -> performSeveritySearch "Severe" mainMenu
        "b" -> mainMenu
        "q" -> return ()
        _ -> do
            putStrLn "Invalid option."
            searchBySeverity mainMenu

performSeveritySearch :: String -> IO () -> IO ()
performSeveritySearch severity mainMenu = do
    results <- DB.getRoadsBySeverity (T.pack severity)
    if null results
        then do
            putStrLn $ "No roads found with severity: " ++ severity
            searchBySeverity mainMenu
        else do
            putStrLn $ "\nRoads with severity: " ++ severity
            printf ("%-5s %-30s %-25s %-30s\n" :: String) ("No." :: String) ("Road Name" :: String) ("Timing" :: String) ("Description" :: String)
            putStrLn $ replicate 90 '-'
            mapM_ (\(i, (_, name, desc, time)) -> printf ("%-5d %-30s %-25s %-30s\n" :: String) (i :: Int) (T.unpack name) (T.unpack time) (T.unpack desc)) (zip [1..] results)
            
            putStrLn "\nm. Main Menu"
            putStrLn "b. Back"
            putStrLn "q. Quit"
            putStrLn "Enter option:"
            option <- getLine
            case option of
                "m" -> mainMenu
                "b" -> searchBySeverity mainMenu
                "q" -> return ()
                _ -> do
                    putStrLn "Invalid option."
                    performSeveritySearch severity mainMenu
