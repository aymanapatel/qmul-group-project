{-# LANGUAGE OverloadedStrings #-}
module Actions.Search (
    searchLoop
) where

import Actions.Search.ByName (searchByName)
import Actions.Search.BySeverity (searchBySeverity)
import Actions.Search.ByCoordinates (searchByCoordinates)

-- | Main loop for the search menu.
searchLoop :: IO ()
searchLoop = do
    putStrLn "\n--- Search Menu ---"
    putStrLn "1. Search by road name"
    putStrLn "2. Search by severity level"
    putStrLn "3. Search by coordinates"
    putStrLn "\nq. Quit"
    putStrLn "\n\nEnter option:"
    option <- getLine
    case option of
        "1" -> searchByName searchLoop
        "2" -> searchBySeverity searchLoop
        "3" -> searchByCoordinates searchLoop
        "q" -> return ()
        _ -> do
            putStrLn "Invalid option."
            searchLoop
