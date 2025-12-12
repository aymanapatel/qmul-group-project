{-# LANGUAGE OverloadedStrings #-}
module Actions.Search.ByName (
    searchByName
) where

import qualified Data.Text as T
import qualified Database as DB
import Actions.Search.Common (displayResults, printRoadStatus, promptContinuation)

searchByName :: IO () -> IO ()
searchByName mainMenu = do
    putStrLn "\nEnter road name or 'b' to go back:"
    queryStr <- getLine
    case queryStr of
        "b" -> mainMenu
        _ -> do
            results <- DB.searchRoads (T.pack queryStr)
            case results of
                [] -> do
                    putStrLn "No Roads found by this name. Showing all roads:"
                    allRoads <- DB.getAllRoads
                    displayResults allRoads (searchByName mainMenu) mainMenu mainMenu
                
                [(rid, name)] -> do
                    putStrLn $ "\nFound one road: " ++ T.unpack name
                    printRoadStatus rid mainMenu
                    promptContinuation mainMenu
                    
                _ -> displayResults results (searchByName mainMenu) mainMenu mainMenu
