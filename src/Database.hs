{-# LANGUAGE OverloadedStrings #-}
module Database (
    createTables,
    saveRoads,
    saveDisruptions,
    searchRoads,
    getLatestStatus,
    getAllRoads,
    getRoadsBySeverity,
    dumpLogs,
    getReliabilityReport,
    getWorstDayAnalysis,
    getWorstHourAnalysis
) where

import Database.SQLite.Simple
import Types
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Text (Text)
import qualified Data.Text as T

-- | Creates the necessary database tables
createTables :: IO ()
createTables = do
    conn <- open "tfl.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS roads (\
                  \id TEXT PRIMARY KEY,\
                  \displayName TEXT NOT NULL,\
                  \url TEXT\
                  \)"
    execute_ conn "CREATE TABLE IF NOT EXISTS road_status_logs (\
                  \id INTEGER PRIMARY KEY AUTOINCREMENT,\
                  \road_id TEXT NOT NULL,\
                  \severity TEXT NOT NULL,\
                  \description TEXT NOT NULL,\
                  \start_date TEXT,\
                  \end_date TEXT,\
                  \timestamp TEXT NOT NULL,\
                  \FOREIGN KEY(road_id) REFERENCES roads(id)\
                  \)"
    execute_ conn "CREATE TABLE IF NOT EXISTS road_disruptions (\
                  \id TEXT PRIMARY KEY,\
                  \url TEXT,\
                  \location TEXT,\
                  \description TEXT,\
                  \status TEXT,\
                  \severity TEXT,\
                  \timestamp TEXT NOT NULL\
                  \)"
    close conn

-- | Saves road information and status logs to the database
saveRoads :: [Road] -> IO ()
saveRoads roads = do
    conn <- open "tfl.db"
    time <- getCurrentTime
    let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" time
    
    -- Insert or ignore roads (static info)
    executeMany conn "INSERT OR REPLACE INTO roads (id, displayName, url) VALUES (?, ?, ?)" 
        (map (\r -> (roadId r, roadDisplayName r, roadUrl r)) roads)
        
    -- Insert status logs
    executeMany conn "INSERT INTO road_status_logs (road_id, severity, description, start_date, end_date, timestamp) VALUES (?, ?, ?, ?, ?, ?)"
        (map (\r -> (roadId r, roadStatusSeverity r, roadStatusSeverityDescription r, roadStatusStartDate r, roadStatusEndDate r, timestamp)) roads)
        
    close conn

-- | Saves disruption information to the database.
saveDisruptions :: [Disruption] -> IO ()
saveDisruptions disruptions = do
    conn <- open "tfl.db"
    time <- getCurrentTime
    let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" time
    
    executeMany conn "INSERT OR REPLACE INTO road_disruptions (id, url, location, description, status, severity, timestamp) VALUES (?, ?, ?, ?, ?, ?, ?)"
        (map (\d -> (disruptionId d, disruptionUrl d, disruptionLocation d, disruptionDescription d, disruptionStatus d, disruptionSeverity d, timestamp)) disruptions)
        
    close conn

-- | Searches for roads by name (partial match).
-- Returns a list of (Road ID, Display Name).
searchRoads :: Text -> IO [(Text, Text)]
searchRoads searchQuery = do
    conn <- open "tfl.db"
    let sql = "SELECT id, displayName FROM roads WHERE displayName LIKE ? OR id LIKE ?"
    let pattern = "%" <> searchQuery <> "%"
    results <- query conn (Query (T.pack sql)) (pattern, pattern) :: IO [(Text, Text)]
    close conn
    return results

-- | Retrieves all roads sorted by display name.
getAllRoads :: IO [(Text, Text)]
getAllRoads = do
    conn <- open "tfl.db"
    let sql = "SELECT id, displayName FROM roads ORDER BY displayName ASC"
    results <- query_ conn (Query (T.pack sql)) :: IO [(Text, Text)]
    close conn
    return results

-- | Retrieves roads by severity with details.
getRoadsBySeverity :: Text -> IO [(Text, Text, Text, Text)]
getRoadsBySeverity severity = do
    conn <- open "tfl.db"
    let sql = "SELECT r.id, r.displayName, l.description, l.timestamp \
              \FROM road_status_logs l \
              \JOIN roads r ON l.road_id = r.id \
              \WHERE l.severity = ? \
              \AND l.timestamp = (SELECT MAX(timestamp) FROM road_status_logs WHERE road_id = r.id) \
              \ORDER BY r.displayName ASC"
    results <- query conn (Query (T.pack sql)) (Only severity) :: IO [(Text, Text, Text, Text)]
    close conn
    return results

-- | Retrieves all status logs from the database
dumpLogs :: IO [RoadStatusLog]
dumpLogs = do
    conn <- open "tfl.db"
    let sql = "SELECT id, road_id, severity, description, start_date, end_date, timestamp FROM road_status_logs"
    logs <- query_ conn (Query (T.pack sql)) :: IO [RoadStatusLog]
    close conn
    return logs

-- | Generates a reliability report based on historical data.
--
-- Calculates the percentage of "Good" status logs for each road.
-- Returns a list of tuples containing:
-- (Road Name, Total Logs, Good Service Count, Reliability Percentage)
getReliabilityReport :: IO [(Text, Int, Int, Double)]
getReliabilityReport = do
    conn <- open "tfl.db"
    let sql = "SELECT r.displayName, \
              \COUNT(l.id) as total, \
              \SUM(CASE WHEN l.severity = 'Good' THEN 1 ELSE 0 END) as good, \
              \(CAST(SUM(CASE WHEN l.severity = 'Good' THEN 1 ELSE 0 END) AS FLOAT) / COUNT(l.id)) * 100 as reliability \
              \FROM roads r \
              \JOIN road_status_logs l ON r.id = l.road_id \
              \GROUP BY r.id \
              \ORDER BY reliability DESC"
    report <- query_ conn (Query (T.pack sql)) :: IO [(Text, Int, Int, Double)]
    close conn
    return report

-- | Analyzes traffic disruptions by day of the week.
--
-- Aggregates logs where severity is NOT 'Good' by day of the week (0-6).
-- Maps the numeric day to a human-readable name (Sunday-Saturday).
-- Returns a list of (Day Name, Disruption Count) sorted by count descending.
getWorstDayAnalysis :: IO [(Text, Int)]
getWorstDayAnalysis = do
    conn <- open "tfl.db"
    let sql = "SELECT strftime('%w', timestamp) as day_num, \
              \COUNT(*) as disruptions \
              \FROM road_status_logs \
              \WHERE severity != 'Good' \
              \GROUP BY day_num \
              \ORDER BY disruptions DESC"
    results <- query_ conn (Query (T.pack sql)) :: IO [(Text, Int)]
    close conn
    return $ map (\(d, c) -> (dayName d, c)) results
  where
    dayName "0" = "Sunday"
    dayName "1" = "Monday"
    dayName "2" = "Tuesday"
    dayName "3" = "Wednesday"
    dayName "4" = "Thursday"
    dayName "5" = "Friday"
    dayName "6" = "Saturday"
    dayName _   = "Unknown"

-- | Analyzes traffic disruptions by hour of the day.
--
-- Aggregates logs where severity is NOT 'Good' by hour (00-23).
-- Returns a list of (Hour, Disruption Count) sorted by count descending.
getWorstHourAnalysis :: IO [(Text, Int)]
getWorstHourAnalysis = do
    conn <- open "tfl.db"
    let sql = "SELECT strftime('%H', timestamp) as hour, \
              \COUNT(*) as disruptions \
              \FROM road_status_logs \
              \WHERE severity != 'Good' \
              \GROUP BY hour \
              \ORDER BY disruptions DESC"
    results <- query_ conn (Query (T.pack sql)) :: IO [(Text, Int)]
    close conn
    return results

-- | Retrieves the latest status for a specific road.
getLatestStatus :: Text -> IO [(Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]
getLatestStatus roadId = do
    conn <- open "tfl.db"
    let sql = "SELECT r.displayName, l.severity, l.description, r.url, l.start_date, l.end_date, l.timestamp \
              \FROM road_status_logs l \
              \JOIN roads r ON l.road_id = r.id \
              \WHERE l.road_id = ? \
              \ORDER BY l.timestamp DESC LIMIT 1"
    results <- query conn (Query (T.pack sql)) (Only roadId) :: IO [(Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]
    close conn
    return results
