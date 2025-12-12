{-# LANGUAGE OverloadedStrings #-}
module Actions.Search.ByName (
    searchByName
) where

import qualified Data.Text as T
import qualified Database as DB
import Actions.Search.Common (displayResults, printRoadStatus, promptContinuation, printSeparator)

searchByName :: IO () -> IO ()
searchByName mainMenu = do
    printSeparator
    putStrLn "\nPlease enter a road name or press 'b' to return:"
    queryStr <- getLine
    case queryStr of
        "b" -> mainMenu
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
                                    displayResults allRoads (searchByName mainMenu) mainMenu mainMenu
                                
                                _ -> displayResults fallbackResults (searchByName mainMenu) mainMenu mainMenu
                        else do
                            -- Empty query case (shouldn't happen with getLine unless just enter)
                            putStrLn "\nNo input provided. Showing all available roads:"
                            allRoads <- DB.getAllRoads
                            displayResults allRoads (searchByName mainMenu) mainMenu mainMenu

                [(rid, name)] -> do
                    putStrLn $ "\nSingle match found: " ++ T.unpack name
                    printRoadStatus rid mainMenu
                    promptContinuation mainMenu
                    
                _ -> displayResults results (searchByName mainMenu) mainMenu mainMenu
