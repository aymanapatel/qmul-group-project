# TfL Road Flow Tracker Report

## Application Overview

The **TfL Road Flow Tracker** is a robust Haskell application designed to harvest, store, monitor, and analyze road status data from the Transport for London (TfL) Unified API. It goes beyond simple data fetching by offering **real-time geospatial recommendations** and **long-term reliability analytics**.

Key capabilities include:

1.  **Historical Data Harvesting**: Builds a persistent SQL database of road status and disruptions.
2.  **Intelligent Search**: Allows users to find roads by name, current severity, or geospatial proximity.
3.  **Smart Recommendations**: A recommendation engine that calculates distances to nearby roads and suggests the optimal route (closest "Good Service" road) while warning against severe disruptions.
4.  **Trend Analysis**: Generates reports on reliability percentages and temporal traffic patterns.

---

## Compilation and Usage

### Prerequisites

- Haskell Stack
- TfL API Key (stored in `.env`)

### Compilation

```bash
stack build
```

### Key Commands

| Command                 | Action                                                                                   |
| :---------------------- | :--------------------------------------------------------------------------------------- |
| `stack run -- create`   | **Initialize**: Creates `tfl.db` with optimized schema (`roads`, `logs`, `disruptions`). |
| `stack run -- loaddata` | **Harvest**: Fetches live data (status + geometry) and updates the database.             |
| `stack run -- search`   | **Explore**: Launches the **Interactive CLI** for searching and recommendations.         |
| `stack run -- report`   | **Analyze**: Displays reliability statistics and "Worst Day/Hour" trends.                |
| `stack run -- dumpdata` | **Export**: Dumps all logs to `data.json`.                                               |

---

## Technical Design & Architecture

### Modular Structure

We adopted a highly modular architecture to ensure separation of concerns and maintainability:

- **`src/Data` & `src/Database`**: Handles SQLite interactions with a normalized schema.
- **`src/Fetch` & `src/Parse`**: Manages HTTP requests and complex JSON parsing (including nested polygon geometries).
- **`src/Actions`**: Contains the core business logic for the interactive CLI, further broken down into:
  - `Search/ByName.hs`: Fuzzy string matching.
  - `Search/BySeverity.hs`: Filtering logic.
  - `Search/ByCoordinates.hs`: The core of the **Geospatial Recommendation Engine**.
- **`src/Utils`**: Helper modules for `Env` loading and `Display` formatting.

### Database Design

The schema is normalized to handle the complexity of TfL data:

- `roads`: Stores static attributes and cached geometry (bounds/envelopes).
- **`road_status_logs`**: Capture time-series data for trend analysis.
- **`road_disruptions`**: Stores complex polygon data for precise disruption mapping.

---

## Advanced Feature: Geospatial Recommendation Engine

The flagship feature of this application is the **Geospatial Recommendation Engine**, which demonstrates significant technical complexity:

1.  **Problem**: A user at a specific location needs to know the best road to take, avoiding disruptions.
2.  **Solution**:
    - **Haversine Implementation**: We implemented the Haversine formula in Haskell to calculate precise distances between the user's coordinates and road midpoints.
    - **Polygon Logic**: The app parses complex polygon bounds to determine if a user's coordinate falls within or near a specific road or disruption zone.
    - **Recommendation Algorithm**:
      - Filters roads within a radius.
      - Sorts by distance.
      - **Smart Selection**: Identifies the _closest_ road that specifically has "Good Service", automatically recommending it over closer roads that might have "Severe Delays".

## Extra Feature: Traffic Trend Analyzer

In addition to the recommendation engine, we implemented a **Traffic Trend Analyzer**:

- **Aggregations**: Uses advanced SQL grouping to calculate reliability percentages.
- **Temporal Analysis**: Maps timestamps to days of the week to identify the "Worst Day" and "Worst Hour" for London traffic, providing actionable insights for journey planning.
