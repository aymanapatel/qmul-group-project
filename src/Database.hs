{-# LANGUAGE OverloadedStrings #-}
module Database (
    createTables,
    saveRoads,
    saveDisruptions,
    searchRoads,
    searchRoadsByFirstChar,
    getLatestStatus,
    getAllRoads,
    getRoadsBySeverity,
    getAllLogs,
    dumpAllData,
    getReliabilityReport,
    getWorstDayAnalysis,
    getWorstHourAnalysis,
    getDisruptionsForRoad,
    getNearestGoodRoads,
    getNearestRoads
) where

import Database.SQLite.Simple
import Types
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Text (Text)
import Data.Aeson (decode)
import Data.List (minimumBy, sortBy, sortOn)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as T

-- | Creates the necessary database tables
createTables :: IO ()
createTables = do
    conn <- open "tfl.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS roads (\
                  \id TEXT PRIMARY KEY,\
                  \displayName TEXT NOT NULL,\
                  \url TEXT,\
                  \bounds TEXT,\
                  \envelope TEXT,\
                  \lat REAL,\
                  \lon REAL\
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
                  \point TEXT,\
                  \geometry TEXT,\
                  \lat REAL,\
                  \lon REAL,\
                  \nearest_road_id TEXT,\
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
    executeMany conn "INSERT OR REPLACE INTO roads (id, displayName, url, bounds, envelope, lat, lon) VALUES (?, ?, ?, ?, ?, ?, ?)" 
        (map (\r -> (roadId r, roadDisplayName r, roadUrl r, roadBounds r, roadEnvelope r, roadLat r, roadLon r)) roads)
        
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
    
    let rows = map (\d -> DisruptionRow d timestamp) disruptions
    executeMany conn "INSERT OR REPLACE INTO road_disruptions (id, url, location, description, status, severity, point, geometry, lat, lon, nearest_road_id, timestamp) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" rows
        
    close conn

data DisruptionRow = DisruptionRow Disruption Text

instance ToRow DisruptionRow where
    toRow (DisruptionRow d timestamp) = toRow d ++ toRow (Only timestamp)

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

-- | Searches for roads by name (first character match).
-- Returns a list of (Road ID, Display Name).
searchRoadsByFirstChar :: Text -> IO [(Text, Text)]
searchRoadsByFirstChar searchQuery = do
    conn <- open "tfl.db"
    let sql = "SELECT id, displayName FROM roads WHERE displayName LIKE ? OR id LIKE ?"
    let pattern = searchQuery <> "%"
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

-- | Retrieves all data from the database.
dumpAllData :: IO DatabaseDump
dumpAllData = do
    conn <- open "tfl.db"
    
    -- Fetch all roads
    roads <- query_ conn "SELECT r.id, r.displayName, '', '', '', '', r.url, r.bounds, r.envelope, r.lat, r.lon FROM roads r" :: IO [Road]
    
    -- Fetch all logs
    logs <- query_ conn "SELECT id, road_id, severity, description, start_date, end_date, timestamp FROM road_status_logs" :: IO [RoadStatusLog]
    
    -- Fetch all disruptions
    disruptions <- query_ conn "SELECT id, url, location, description, status, severity, point, geometry, lat, lon, nearest_road_id FROM road_disruptions" :: IO [Disruption]
    
    close conn
    return $ DatabaseDump roads logs disruptions

-- | Retrieves all status logs from the database
getAllLogs :: IO [RoadStatusLog]
getAllLogs = do
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
getLatestStatus rId = do
    conn <- open "tfl.db"
    let sql = "SELECT r.displayName, l.severity, l.description, r.url, l.start_date, l.end_date, l.timestamp \
              \FROM road_status_logs l \
              \JOIN roads r ON l.road_id = r.id \
              \WHERE l.road_id = ? \
              \ORDER BY l.timestamp DESC LIMIT 1"
    results <- query conn (Query (T.pack sql)) (Only rId) :: IO [(Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]
    close conn
    return results

-- | Retrieves disruptions near a specific road.
getDisruptionsForRoad :: Text -> IO [Disruption]
getDisruptionsForRoad rId = do
    conn <- open "tfl.db"
    let sql = "SELECT id, url, location, description, status, severity, point, geometry, lat, lon, nearest_road_id \
              \FROM road_disruptions \
              \WHERE nearest_road_id = ?"
    results <- query conn (Query (T.pack sql)) (Only rId) :: IO [Disruption]
    close conn
    return results

-- | Finds the nearest roads with "Good" status.
-- Uses a simple Euclidean distance approximation on lat/lon.
getNearestGoodRoads :: Double -> Double -> Int -> IO [(Text, Text, Double)]
getNearestGoodRoads lat lon limit = do
    conn <- open "tfl.db"
    -- SQLite doesn't have SQRT or POW by default, so we fetch all good roads and sort in Haskell
    -- Or we can approximate with (lat-lat)^2 + (lon-lon)^2 order if we fetch enough candidates.
    -- Fetching all "Good" roads is safer for small datasets like this.
    let sql = "SELECT r.id, r.displayName, r.lat, r.lon \
              \FROM roads r \
              \JOIN road_status_logs l ON r.id = l.road_id \
              \WHERE l.severity = 'Good' \
              \AND l.timestamp = (SELECT MAX(timestamp) FROM road_status_logs WHERE road_id = r.id) \
              \AND r.lat IS NOT NULL AND r.lon IS NOT NULL"
    rows <- query_ conn (Query (T.pack sql)) :: IO [(Text, Text, Double, Double)]
    close conn
    
    let sorted = take limit $ sortOnDistance lat lon rows
    return $ map (\(rid, name, _, _) -> (rid, name, 0.0)) sorted -- Distance not strictly needed for display, just order

-- | Finds the nearest roads (regardless of status) using Bounding Box logic.
-- Returns (Road ID, Display Name, Distance, Severity, Status Description)
getNearestRoads :: Double -> Double -> Int -> IO [(Text, Text, Double, Text, Text)]
getNearestRoads lat lon limit = do
    conn <- open "tfl.db"
    -- Join with logs to get latest severity and description.
    let sql = "SELECT r.id, r.displayName, r.bounds, r.envelope, r.lat, r.lon, \
              \COALESCE(l.severity, 'Unknown'), COALESCE(l.description, 'Unknown') \
              \FROM roads r \
              \LEFT JOIN road_status_logs l ON r.id = l.road_id AND l.timestamp = (SELECT MAX(timestamp) FROM road_status_logs WHERE road_id = r.id) \
              \WHERE r.lat IS NOT NULL AND r.lon IS NOT NULL"
    rows <- query_ conn (Query (T.pack sql)) :: IO [(Text, Text, Maybe Text, Maybe Text, Double, Double, Text, Text)]
    close conn
    
    let withDist = map (\(rid, name, bounds, envelope, rLat, rLon, sev, desc) -> 
            let dist = calculateDistance lat lon bounds envelope rLat rLon
            in (rid, name, dist, sev, desc)) rows
            
    let sorted = take limit $ sortBy (\(_,_,d1,_,_) (_,_,d2,_,_) -> compare d1 d2) withDist
    return sorted

calculateDistance :: Double -> Double -> Maybe Text -> Maybe Text -> Double -> Double -> Double
calculateDistance lat lon bounds envelope rLat rLon =
    let box = case bounds of
            Just b -> parseBounds b
            Nothing -> case envelope of
                Just e -> parseBounds e
                Nothing -> []
        
        targetPoint = if null box
                      then (rLat, rLon)
                      else closestPointInPolygon lat lon box
    in haversine lat lon (fst targetPoint) (snd targetPoint)
  where
    parseBounds :: Text -> [[Double]]
    parseBounds jsonStr = case decode (LBS.fromStrict $ T.encodeUtf8 jsonStr) :: Maybe [[Double]] of
        Just points -> points
        Nothing -> []

-- | Finds the closest point in a polygon (vertices) to a given point.
-- If the point is inside the bounding box, returns the point itself.
closestPointInPolygon :: Double -> Double -> [[Double]] -> (Double, Double)
closestPointInPolygon lat lon points = 
    let validPoints = [ (l, t) | [l, t] <- points ] -- Expecting [lon, lat] format implies 2 elements
        lats = map snd validPoints
        lons = map fst validPoints
        
        minLat = if null lats then lat else minimum lats
        maxLat = if null lats then lat else maximum lats
        minLon = if null lons then lon else minimum lons
        maxLon = if null lons then lon else maximum lons
    in if lat >= minLat && lat <= maxLat && lon >= minLon && lon <= maxLon
        then (lat, lon) -- Inside the bounding box
        else 
            -- Find the vertex with minimum squared Euclidean distance
            let squaredDist (ln, lt) = (lt - lat) ^ (2 :: Int) + (ln - lon) ^ (2 :: Int)
                closest = if null validPoints 
                          then (0, 0) 
                          else minimumBy (\p1 p2 -> compare (squaredDist p1) (squaredDist p2)) validPoints
            in (snd closest, fst closest)

-- | Calculates the Haversine distance between two points in miles.
haversine :: Double -> Double -> Double -> Double -> Double
haversine lat1 lon1 lat2 lon2 = d
  where
    r = 3958.8 -- Earth radius in miles
    toRadians deg = deg * pi / 180
    dLat = toRadians (lat2 - lat1)
    dLon = toRadians (lon2 - lon1)
    lat1' = toRadians lat1
    lat2' = toRadians lat2
    a = sin (dLat / 2) ^ (2 :: Int) + cos lat1' * cos lat2' * sin (dLon / 2) ^ (2 :: Int)
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    d = r * c

-- | Helper to sort by squared Euclidean distance (still useful for rough initial sorting if needed, but we use calculated Haversine now)
sortOnDistance :: Double -> Double -> [(Text, Text, Double, Double)] -> [(Text, Text, Double, Double)]
sortOnDistance lat lon = sortOn (\(_, _, rLat, rLon) -> (rLat - lat) ^ (2 :: Int) + (rLon - lon) ^ (2 :: Int))
