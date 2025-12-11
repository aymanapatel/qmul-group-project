{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (encode)
import Fetch
import Parse
import Database
import Text.Printf (printf)

import qualified Data.Text as T
import Utils.Env (loadEnv, getEnvVar)
import Actions.Search (searchLoop)

main :: IO ()
main = do
    args <- getArgs
    env <- loadEnv
    let apiKey = getEnvVar "TFL_APP_KEY" env
    
    case args of
        ["create"] -> do
            putStrLn "Creating database tables..."
            createTables
            putStrLn "Done."
            
        ["loaddata"] -> do
            if null apiKey
                then putStrLn "Error: TFL_APP_KEY not found in .env"
                else do
                    putStrLn "Downloading road data..."
                    roadResult <- downloadRoads apiKey
                    case roadResult of
                        Left err -> putStrLn err
                        Right jsonRoads -> do
                            case parseRoads jsonRoads of
                                Left err -> putStrLn $ "Error parsing roads: " ++ err
                                Right roads -> do
                                    putStrLn "Processing road geometry..."
                                    let processedRoads = processRoads roads
                                    putStrLn $ "Saving " ++ show (length processedRoads) ++ " roads to database..."
                                    saveRoads processedRoads
                                    
                                    putStrLn "Downloading disruption data..."
                                    disruptionResult <- downloadDisruptions apiKey
                                    case disruptionResult of
                                        Left err -> putStrLn err
                                        Right jsonDisruptions -> do
                                            case parseDisruptions jsonDisruptions of
                                                Left err -> putStrLn $ "Error parsing disruptions: " ++ err
                                                Right disruptions -> do
                                                    putStrLn "Processing disruptions and finding nearest roads..."
                                                    let processedDisruptions = processDisruptions disruptions processedRoads
                                                    putStrLn $ "Saving " ++ show (length processedDisruptions) ++ " disruptions to database..."
                                                    saveDisruptions processedDisruptions
                                    putStrLn "Done."
            
        ["dumpdata"] -> do
            putStrLn "Dumping COMPLETE database to data.json..."
            fullDump <- dumpAllData
            LBS.writeFile "data.json" (encode fullDump)
            putStrLn "Done."
            
        ["search"] -> searchLoop

        ["report"] -> do
            putStrLn "Generating Road Reliability Report..."
            report <- getReliabilityReport
            putStrLn "----------------------------------------------------------------"
            printf ("%-25s | %-10s | %-10s | %-10s\n" :: String) ("Road Name" :: String) ("Total Logs" :: String) ("Good Svc" :: String) ("Reliability" :: String)
            putStrLn "----------------------------------------------------------------"
            mapM_ (\(name, total, good, reliability) -> 
                printf ("%-25s | %-10d | %-10d | %-9.1f%%\n" :: String) (T.unpack name) total good reliability) report
            putStrLn "----------------------------------------------------------------"
            
            putStrLn "\nTraffic Trend Analysis (Worst Disruption Times):"
            
            putStrLn "\nBy Day of Week:"
            putStrLn "----------------------"
            days <- getWorstDayAnalysis
            if null days
                then putStrLn "No disruption data available."
                else mapM_ (\(day, count) -> printf "%-10s: %d disruptions\n" (T.unpack day) count) days
                
                
            putStrLn "\nBy Hour of Day:"
            putStrLn "----------------------"
            hours <- getWorstHourAnalysis
            if null hours
                then putStrLn "No disruption data available."
                else mapM_ (\(hour, count) -> printf "%-10s: %d disruptions\n" (T.unpack hour) count) hours
        
        _ -> putStrLn "Usage: stack run -- [create|loaddata|dumpdata|report|search]"
