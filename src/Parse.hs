module Parse (
    parseRoads,
    parseDisruptions,
    processRoads,
    processDisruptions
) where

import Types
import Data.Aeson
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.List (minimumBy)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T

-- | This function parses JSON data into a list of 'Road' objects.
--
-- Uses Aeson's 'eitherDecode' to parse the Lazy ByteString.
-- Returns 'Left errorMsg' on failure, or 'Right [Road]' on success.
parseRoads :: LBS.ByteString -- ^ The raw JSON data
           -> Either String [Road] -- ^ The result of parsing
parseRoads json = eitherDecode json

-- | This function parses JSON data into a list of 'Disruption' objects.
--
-- Uses Aeson's 'eitherDecode' to parse the Lazy ByteString.
-- Returns 'Left errorMsg' on failure, or 'Right [Disruption]' on success.
parseDisruptions :: LBS.ByteString -- ^ The raw JSON data
                 -> Either String [Disruption] -- ^ The result of parsing
parseDisruptions json = eitherDecode json


-- | This function processes roads to calculate their center coordinates from bounds/envelope.
processRoads :: [Road] -> [Road]
processRoads = map calculateRoadCenter

-- | This function calculates the center (lat/lon) of a road based on its envelope or bounds.
calculateRoadCenter :: Road -> Road
calculateRoadCenter road = 
    case (roadEnvelope road, roadBounds road) of
        (Just env, _) -> parseAndSetCenter env road
        (_, Just bnds) -> parseAndSetCenter bnds road
        _ -> road
  where
    parseAndSetCenter :: Text -> Road -> Road
    parseAndSetCenter jsonStr r = 
        -- API returns bounds as [[lon, lat], [lon, lat]]
        case decode (LBS.fromStrict $ T.encodeUtf8 jsonStr) :: Maybe [[Double]] of
            Just points | not (null points) -> 
                let validPoints = [ (l, t) | [l, t] <- points ] -- Expecting [lon, lat] implies 2 elements
                    lats = map snd validPoints
                    lons = map fst validPoints
                in if null lats || null lons
                   then r
                   else 
                       let minLat = minimum lats
                           maxLat = maximum lats
                           minLon = minimum lons
                           maxLon = maximum lons
                       in r { roadLat = Just ((minLat + maxLat) / 2)
                            , roadLon = Just ((minLon + maxLon) / 2) 
                            }
            _ -> r

-- | This function processes disruptions to calculate their coordinates and find the nearest road.
processDisruptions :: [Disruption] -> [Road] -> [Disruption]
processDisruptions disruptions roads = map (findNearestRoad roads . calculateDisruptionCenter) disruptions

-- | This function calculates the center of a disruption from its point geometry.
calculateDisruptionCenter :: Disruption -> Disruption
calculateDisruptionCenter disruption = 
    case disruptionPoint disruption of
        Just pointStr -> 
            -- Assuming point is a JSON string representing "[lat, lon]" or similar
            -- TfL API often returns "point": "[51.5, -0.1]" stringified
            case decode (LBS.fromStrict $ T.encodeUtf8 pointStr) :: Maybe [Double] of
                Just [lat, lon] -> disruption { disruptionLat = Just lat, disruptionLon = Just lon }
                _ -> disruption
        Nothing -> disruption

-- | This function finds the nearest road for a given disruption.
findNearestRoad :: [Road] -> Disruption -> Disruption
findNearestRoad roads disruption = 
    case (disruptionLat disruption, disruptionLon disruption) of
        (Just dLat, Just dLon) -> 
            let roadsWithCoords = filter (\r -> isJust (roadLat r) && isJust (roadLon r)) roads
            in case roadsWithCoords of
                [] -> disruption
                _ -> let nearest = minimumBy (compareDistance dLat dLon) roadsWithCoords
                     in disruption { disruptionNearestRoadId = Just (roadId nearest) }
        _ -> disruption

-- | This function compares the distance of two roads to a point (dLat, dLon).
compareDistance :: Double -> Double -> Road -> Road -> Ordering
compareDistance dLat dLon r1 r2 = 
    compare (dist r1) (dist r2)
  where
    dist r = case (roadLat r, roadLon r) of
        (Just rLat, Just rLon) -> (rLat - dLat) ^ (2 :: Int) + (rLon - dLon) ^ (2 :: Int) -- Squared Euclidean distance is sufficient for comparison
        _ -> 1/0 -- Infinity
