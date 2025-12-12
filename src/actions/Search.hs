{-# LANGUAGE OverloadedStrings #-}
module Actions.Search (
    searchLoop
) where

import Actions.Search.ByName (searchByName)
import Actions.Search.BySeverity (searchBySeverity)
import Actions.Search.ByCoordinates (searchByCoordinates)
import Actions.Search.Common (exitApp, printSeparator)
import Utils.Display (box)

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
        "1" -> searchByName runSearchMenu
        "2" -> searchBySeverity runSearchMenu
        "3" -> searchByCoordinates runSearchMenu
        "exit" -> exitApp
        _ -> do
            putStrLn "\nPlease select an option from the given list."
            runSearchMenu
