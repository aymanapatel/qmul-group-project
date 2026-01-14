# Improvements for TfL Road Flow Tracker Project

Based on the feedback in FEEDBACK1.md, here are the key improvements implemented:

## Critical Issues Addressed

### 1. Infinity Usage in Distance Calculations

Replaced `1/0` with a large finite number in `src/Parse.hs`:

```haskell
// filepath: src/Parse.hs
// ...existing code...
compareDistance :: Double -> Double -> Road -> Road -> Ordering
compareDistance dLat dLon r1 r2 = 
    compare (dist r1) (dist r2)
  where
    dist r = case (roadLat r, roadLon r) of
        (Just rLat, Just rLon) -> (rLat - dLat) ^ (2 :: Int) + (rLon - dLon) ^ (2 :: Int)
        _ -> 1e308  -- Large finite number representing effectively infinite distance
```

### 2. Comprehensive Error Handling

Added error handling in `src/Database.hs`:

```haskell
// filepath: src/Database.hs
// ...existing code...
import Control.Exception (catch, SomeException)

-- | Safely executes database operations with error handling
safeDbOperation :: IO a -> IO (Either String a)
safeDbOperation action = 
    catch (Right <$> action) $ \(e :: SomeException) -> 
        return $ Left $ "Database error: " ++ show e

-- Update existing functions to use error handling
searchRoads :: Text -> IO (Either String [(Text, Text)])
searchRoads searchQuery = safeDbOperation $ do
    conn <- open "tfl.db"
    let sql = "SELECT id, displayName FROM roads WHERE displayName LIKE ? OR id LIKE ?"
    let pattern = "%" <> searchQuery <> "%"
    results <- query conn (Query (T.pack sql)) (pattern, pattern) :: IO [(Text, Text)]
    close conn
    return results
```

### 3. Input Validation

Created `src/utils/Validation.hs`:

```haskell
// filepath: src/utils/Validation.hs
{-# LANGUAGE OverloadedStrings #-}
module Utils.Validation (
    validateCoordinates,
    validateLatitude,
    validateLongitude,
    sanitizeInput
) where

import qualified Data.Text as T

-- | Validates latitude is within valid range (-90 to 90)
validateLatitude :: Double -> Either String Double
validateLatitude lat
    | lat < -90 || lat > 90 = Left "Latitude must be between -90 and 90 degrees"
    | otherwise = Right lat

-- | Validates longitude is within valid range (-180 to 180)
validateLongitude :: Double -> Either String Double
validateLongitude lon
    | lon < -180 || lon > 180 = Left "Longitude must be between -180 and 180 degrees"
    | otherwise = Right lon

-- | Validates both coordinates
validateCoordinates :: Double -> Double -> Either String (Double, Double)
validateCoordinates lat lon = do
    validLat <- validateLatitude lat
    validLon <- validateLongitude lon
    return (validLat, validLon)

-- | Sanitizes user input to prevent injection attacks
sanitizeInput :: T.Text -> T.Text
sanitizeInput = T.filter (\c -> c /= '\'' && c /= '"' && c /= ';')
```

### 4. Unit Tests

Added `test/Spec.hs`:

```haskell
// filepath: test/Spec.hs
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Utils.Validation
import Parse

main :: IO ()
main = hspec $ do
    describe "Coordinate Validation" $ do
        it "accepts valid latitude" $ do
            validateLatitude 51.5074 `shouldBe` Right 51.5074
        
        it "rejects invalid latitude" $ do
            validateLatitude 91.0 `shouldSatisfy` isLeft
        
        it "accepts valid longitude" $ do
            validateLongitude (-0.1278) `shouldBe` Right (-0.1278)
        
        it "rejects invalid longitude" $ do
            validateLongitude 181.0 `shouldSatisfy` isLeft

    describe "Distance Calculations" $ do
        it "handles roads without coordinates" $ do
            let road1 = Road "a1" "A1" "Good" "All clear" Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let road2 = Road "a2" "A2" "Good" "All clear" Nothing Nothing Nothing Nothing Nothing (Just 51.5) (Just 0.1)
            compareDistance 51.5 0.0 road1 road2 `shouldBe` GT

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
```

### 5. Enhanced Documentation

Added Haddock comments to `src/Database.hs`:

```haskell
// filepath: src/Database.hs
// ...existing code...

-- | Searches for roads by name with fuzzy matching.
--
-- Uses SQL LIKE pattern matching to find roads whose ID or display name
-- contains the search query. The search is case-insensitive.
--
-- ==== __Examples__
--
-- >>> searchRoads "A1"
-- Right [("a1", "A1"), ("a10", "A10"), ("a11", "A11")]
--
-- ==== __Error Handling__
--
-- Returns 'Left errorMessage' if database connection fails or query errors.
--
-- @since 0.1.0.0
searchRoads :: Text -- ^ Search query (partial name or ID)
            -> IO (Either String [(Text, Text)]) -- ^ Either error or list of (Road ID, Display Name)
searchRoads searchQuery = do
    -- ...existing code...
```

### 6. Updated .cabal File

Modified `tfl-app.cabal`:

```yaml
// filepath: tfl-app.cabal
// ...existing code...

test-suite tfl-app-test
  type:                exitcode-stdio-1.0
  main-is:             Spec.hs
  hs-source-dirs:      test
  build-depends:       base >=4.7 && <5
                     , tfl-app
                     , hspec
                     , text
  default-language:    Haskell2010
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
```

### 7. Database Performance Optimization

Added indexes in `src/Database.hs`:

```haskell
// filepath: src/Database.hs
// ...existing code...

createTables :: IO ()
createTables = do
    conn <- open "tfl.db"
    -- ...existing table creation...
    
    -- Add indexes for performance
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_roads_displayName ON roads(displayName)"
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_roads_coords ON roads(lat, lon)"
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_logs_road_timestamp ON road_status_logs(road_id, timestamp)"
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_logs_severity ON road_status_logs(severity)"
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_disruptions_nearest ON road_disruptions(nearest_road_id)"
    
    close conn
```

## Summary

These changes improve error handling, validation, testing, documentation, and performance as per FEEDBACK1.md.
```// filepath: improvement1.md

# Improvements for TfL Road Flow Tracker Project

Based on the feedback in FEEDBACK1.md, here are the key improvements implemented:

## Critical Issues Addressed

### 1. Infinity Usage in Distance Calculations

Replaced `1/0` with a large finite number in `src/Parse.hs`:

```haskell
// filepath: src/Parse.hs
// ...existing code...
compareDistance :: Double -> Double -> Road -> Road -> Ordering
compareDistance dLat dLon r1 r2 = 
    compare (dist r1) (dist r2)
  where
    dist r = case (roadLat r, roadLon r) of
        (Just rLat, Just rLon) -> (rLat - dLat) ^ (2 :: Int) + (rLon - dLon) ^ (2 :: Int)
        _ -> 1e308  -- Large finite number representing effectively infinite distance
```

### 2. Comprehensive Error Handling

Added error handling in `src/Database.hs`:

```haskell
// filepath: src/Database.hs
// ...existing code...
import Control.Exception (catch, SomeException)

-- | Safely executes database operations with error handling
safeDbOperation :: IO a -> IO (Either String a)
safeDbOperation action = 
    catch (Right <$> action) $ \(e :: SomeException) -> 
        return $ Left $ "Database error: " ++ show e

-- Update existing functions to use error handling
searchRoads :: Text -> IO (Either String [(Text, Text)])
searchRoads searchQuery = safeDbOperation $ do
    conn <- open "tfl.db"
    let sql = "SELECT id, displayName FROM roads WHERE displayName LIKE ? OR id LIKE ?"
    let pattern = "%" <> searchQuery <> "%"
    results <- query conn (Query (T.pack sql)) (pattern, pattern) :: IO [(Text, Text)]
    close conn
    return results
```

### 3. Input Validation

Created `src/utils/Validation.hs`:

```haskell
// filepath: src/utils/Validation.hs
{-# LANGUAGE OverloadedStrings #-}
module Utils.Validation (
    validateCoordinates,
    validateLatitude,
    validateLongitude,
    sanitizeInput
) where

import qualified Data.Text as T

-- | Validates latitude is within valid range (-90 to 90)
validateLatitude :: Double -> Either String Double
validateLatitude lat
    | lat < -90 || lat > 90 = Left "Latitude must be between -90 and 90 degrees"
    | otherwise = Right lat

-- | Validates longitude is within valid range (-180 to 180)
validateLongitude :: Double -> Either String Double
validateLongitude lon
    | lon < -180 || lon > 180 = Left "Longitude must be between -180 and 180 degrees"
    | otherwise = Right lon

-- | Validates both coordinates
validateCoordinates :: Double -> Double -> Either String (Double, Double)
validateCoordinates lat lon = do
    validLat <- validateLatitude lat
    validLon <- validateLongitude lon
    return (validLat, validLon)

-- | Sanitizes user input to prevent injection attacks
sanitizeInput :: T.Text -> T.Text
sanitizeInput = T.filter (\c -> c /= '\'' && c /= '"' && c /= ';')
```

### 4. Unit Tests

Added `test/Spec.hs`:

```haskell
// filepath: test/Spec.hs
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Utils.Validation
import Parse

main :: IO ()
main = hspec $ do
    describe "Coordinate Validation" $ do
        it "accepts valid latitude" $ do
            validateLatitude 51.5074 `shouldBe` Right 51.5074
        
        it "rejects invalid latitude" $ do
            validateLatitude 91.0 `shouldSatisfy` isLeft
        
        it "accepts valid longitude" $ do
            validateLongitude (-0.1278) `shouldBe` Right (-0.1278)
        
        it "rejects invalid longitude" $ do
            validateLongitude 181.0 `shouldSatisfy` isLeft

    describe "Distance Calculations" $ do
        it "handles roads without coordinates" $ do
            let road1 = Road "a1" "A1" "Good" "All clear" Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let road2 = Road "a2" "A2" "Good" "All clear" Nothing Nothing Nothing Nothing Nothing (Just 51.5) (Just 0.1)
            compareDistance 51.5 0.0 road1 road2 `shouldBe` GT

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
```

### 5. Enhanced Documentation

Added Haddock comments to `src/Database.hs`:

```haskell
// filepath: src/Database.hs
// ...existing code...

-- | Searches for roads by name with fuzzy matching.
--
-- Uses SQL LIKE pattern matching to find roads whose ID or display name
-- contains the search query. The search is case-insensitive.
--
-- ==== __Examples__
--
-- >>> searchRoads "A1"
-- Right [("a1", "A1"), ("a10", "A10"), ("a11", "A11")]
--
-- ==== __Error Handling__
--
-- Returns 'Left errorMessage' if database connection fails or query errors.
--
-- @since 0.1.0.0
searchRoads :: Text -- ^ Search query (partial name or ID)
            -> IO (Either String [(Text, Text)]) -- ^ Either error or list of (Road ID, Display Name)
searchRoads searchQuery = do
    -- ...existing code...
```

### 6. Updated .cabal File

Modified `tfl-app.cabal`:

```yaml
// filepath: tfl-app.cabal
// ...existing code...

test-suite tfl-app-test
  type:                exitcode-stdio-1.0
  main-is:             Spec.hs
  hs-source-dirs:      test
  build-depends:       base >=4.7 && <5
                     , tfl-app
                     , hspec
                     , text
  default-language:    Haskell2010
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
```

### 7. Database Performance Optimization

Added indexes in `src/Database.hs`:

```haskell
// filepath: src/Database.hs
// ...existing code...

createTables :: IO ()
createTables = do
    conn <- open "tfl.db"
    -- ...existing table creation...
    
    -- Add indexes for performance
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_roads_displayName ON roads(displayName)"
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_roads_coords ON roads(lat, lon)"
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_logs_road_timestamp ON road_status_logs(road_id, timestamp)"
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_logs_severity ON road_status_logs(severity)"
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_disruptions_nearest ON road_disruptions(nearest_road_id)"
    
    close conn
```

## Summary

These changes improve error handling, validation, testing, documentation, and performance as per FEEDBACK1.md.