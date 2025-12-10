{-# LANGUAGE OverloadedStrings #-}
module Types (
    Road(..),
    RoadStatusLog(..),
    Disruption(..),
    CoordinateEntry(..)
) where

import Data.Aeson
import Data.Aeson.Types (Parser, Value)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple (field, Only(..))
import Data.Text (Text)

-- | Represents a Road from the TfL API.
-- Contains the unique identifier, display name, and current status severity.
data Road = Road
    { roadId :: Text -- ^ The unique identifier of the road (e.g., "a1")
    , roadDisplayName :: Text -- ^ The human-readable name of the road (e.g., "A1")
    , roadStatusSeverity :: Text -- ^ The severity of the current status (e.g., "Good", "Serious Delays")
    , roadStatusSeverityDescription :: Text -- ^ A detailed description of the status severity
    , roadStatusStartDate :: Maybe Text -- ^ Start date of the status
    , roadStatusEndDate :: Maybe Text -- ^ End date of the status
    , roadUrl :: Maybe Text -- ^ The URL for more details
    , roadBounds :: Maybe Text -- ^ The bounds of the road (JSON string)
    , roadEnvelope :: Maybe Text -- ^ The envelope of the road (JSON string)
    , roadLat :: Maybe Double -- ^ Calculated center latitude
    , roadLon :: Maybe Double -- ^ Calculated center longitude
    } deriving (Show, Eq)

instance FromJSON Road where
    parseJSON = withObject "Road" $ \v -> Road
        <$> v .: "id"
        <*> v .: "displayName"
        <*> v .: "statusSeverity"
        <*> v .: "statusSeverityDescription"
        <*> v .:? "statusAggregationStartDate"
        <*> v .:? "statusAggregationEndDate"
        <*> v .:? "url"
        <*> v .:? "bounds"
        <*> v .:? "envelope"
        <*> pure Nothing -- lat calculated later
        <*> pure Nothing -- lon calculated later

-- | Represents a historical log of a road's status.
-- Stored in the database for trend analysis.
data RoadStatusLog = RoadStatusLog
    { logId :: Maybe Int -- ^ The primary key in the database (Nothing for new logs)
    , logRoadId :: Text -- ^ Foreign key referencing the Road's ID
    , logStatusSeverity :: Text -- ^ The severity status at the time of logging
    , logStatusDescription :: Text -- ^ The description at the time of logging
    , logStatusStartDate :: Maybe Text -- ^ Start date of the status
    , logStatusEndDate :: Maybe Text -- ^ End date of the status
    , logTimestamp :: Text -- ^ The timestamp of the log in ISO8601 format
    } deriving (Show, Eq)

instance ToJSON RoadStatusLog where
    toJSON (RoadStatusLog lid rid severity desc start end time) = object
        [ "id" .= lid
        , "roadId" .= rid
        , "statusSeverity" .= severity
        , "statusDescription" .= desc
        , "statusStartDate" .= start
        , "statusEndDate" .= end
        , "timestamp" .= time
        ]

instance FromRow Road where
  fromRow = Road <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Road where
  toRow (Road rid name severity desc start end url bounds env lat lon) = 
    toRow (rid, name, severity, desc, start, end, url, bounds, env) ++ toRow (lat, lon)

instance FromRow RoadStatusLog where
  fromRow = RoadStatusLog <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow RoadStatusLog where
  toRow (RoadStatusLog lid rid severity desc start end time) = toRow (lid, rid, severity, desc, start, end, time)

-- | Represents a disruption on the road network.
data Disruption = Disruption
    { disruptionId :: Text -- ^ Unique identifier
    , disruptionUrl :: Text -- ^ URL for more info
    , disruptionLocation :: Text -- ^ Location description
    , disruptionDescription :: Text -- ^ Detailed description
    , disruptionStatus :: Text -- ^ Current status
    , disruptionSeverity :: Text -- ^ Severity level
    , disruptionPoint :: Maybe Text -- ^ The point geometry (JSON string)
    , disruptionGeometry :: Maybe Text -- ^ The full geometry (JSON string)
    , disruptionLat :: Maybe Double -- ^ Latitude
    , disruptionLon :: Maybe Double -- ^ Longitude
    , disruptionNearestRoadId :: Maybe Text -- ^ ID of the nearest road
    } deriving (Show, Eq)

instance FromJSON Disruption where
    parseJSON = withObject "Disruption" $ \v -> Disruption
        <$> v .:? "id" .!= ""
        <*> v .:? "url" .!= ""
        <*> v .:? "location" .!= ""
        <*> v .:? "description" .!= ""
        <*> v .:? "status" .!= ""
        <*> v .:? "severity" .!= ""
        <*> v .:? "point"
        <*> (fmap (T.pack . LBS.unpack . encode) <$> (v .:? "geometry" :: Parser (Maybe Value)))
        <*> pure Nothing -- lat calculated later
        <*> pure Nothing -- lon calculated later
        <*> pure Nothing -- nearestRoadId calculated later

instance ToRow Disruption where
    toRow (Disruption did url loc desc status sev point geom lat lon nrid) = 
        toRow (did, url, loc, desc, status, sev, point, geom, lat, lon) ++ toRow (Only nrid)

instance FromRow Disruption where
    fromRow = Disruption <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- | Represents a pre-defined coordinate entry for search.
data CoordinateEntry = CoordinateEntry
    { coordCorridorName :: Text
    , coordPersonName :: Text
    , coordLat :: Double
    , coordLong :: Double
    , coordArea :: Text
    } deriving (Show, Eq)

instance FromJSON CoordinateEntry where
    parseJSON = withObject "CoordinateEntry" $ \v -> CoordinateEntry
        <$> v .: "corridorName"
        <*> v .: "personName"
        <*> v .: "lat"
        <*> v .: "long"
        <*> v .: "area"
