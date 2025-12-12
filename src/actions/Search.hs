{-# LANGUAGE OverloadedStrings #-}
module Actions.Search (
    searchLoop
) where

import Actions.Search.ByName (searchByName)
import Actions.Search.BySeverity (searchBySeverity)
import Actions.Search.ByCoordinates (searchByCoordinates)
import Actions.Search.Common (exitApp, printSeparator)
import Utils.Display (box)

-- | Main loop for the search menu.
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
        "1" -> searchByName searchLoop
        "2" -> searchBySeverity searchLoop
        "3" -> searchByCoordinates searchLoop
        "exit" -> exitApp
        _ -> do
            putStrLn "\nPlease select an option from the given list."
            searchLoop
