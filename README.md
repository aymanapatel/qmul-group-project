# TfL Road Flow Tracker

## Group Coursework — ECS713U/P Functional Programming 2025/26

**TfL Road Flow Tracker** is a comprehensive Haskell application designed to monitor, analyze, and visualize road status data from Transport for London (TfL). It leverages the TfL Unified API to build a historical database of traffic disruptions, enabling users to check real-time status, find nearest clear routes, and analyze long-term reliability trends.

---

## 🚀 Key Features

- **Real-time Data Harvesting**: Fetches live road status, severity, and disruption polygons from the TfL API.
- **Persistent Storage**: Stores data in a normalized SQLite database for historical analysis.
- **Interactive Search CLI**: A robust menu-driven interface to explore data:
  - **Search by Name**: Fuzzy search for roads (e.g., "A1", "North Circular").
  - **Search by Severity**: meaningful filtering (e.g., find all roads with "Severe Delays").
  - **Geospatial Search**: Find roads near specific coordinates or predefined landmarks.
- **Intelligent Recommendations**:
  - Calculates distances using the **Haversine formula**.
  - Suggests the **"Best Option"** (closest road with "Good Service").
  - Warns against the **"Worst Option"** (roads with severe disruptions).
- **Trend Analytics**: Generates reliability reports identifying the "worst" days and hours for traffic.

---

## 🛠️ Installation & Setup

### Prerequisites

- **Haskell Stack**: Ensure `stack` is installed.
- **TfL API Key**:
  1.  Register at [api-portal.tfl.gov.uk](https://api-portal.tfl.gov.uk/).
  2.  Create a `.env` file in the project root:
      ```env
      TFL_APP_KEY=your_primary_key_here
      ```

### Compilation

Build the project using Stack:

```bash
stack build
```

---

## 📖 Usage Guide

The application is executed via `stack run` followed by a command.

### 1. Database Initialization

First, set up the SQLite database schema (`tfl.db`).

```bash
stack run -- create
```

### 2. Data Harvesting

Fetch live data from TfL. Run this periodically to build a history.

```bash
stack run -- loaddata
```

### 3. Interactive Search (Main Feature)

Launch the interactive CLI to explore the data.

```bash
stack run -- search
```

#### **Search Flows**

- **Option 1: Search by Road Name**

  - Enter a partial name (e.g., "A12").
  - Select from the matching results.
  - View detailed status, severity description, and active disruptions.

- **Option 2: Search by Severity**

  - Choose a severity level (e.g., "Severe Delays", "Closure").
  - See a list of all affected roads sorted by name.

- **Option 3: Search by Coordinate (Geospatial)**
  - **Predefined Locations**: Select a person/location from `coordinates.json` (e.g., "Varsha @ Aldgate").
  - **Manual Input**: Enter custom Latitude and Longitude.
  - **Results**:
    - Displays nearest roads with **distance in miles**.
    - **Recommendation Engine**: Automatically highlights the closest road with "Good Service" vs. roads to avoid.
    - Detailed breakdown of nearest disruptions.

### 4. Analytics Report

Generate statistical insights on road reliability.

```bash
stack run -- report
```

- **Reliability Score**: % of time a road has "Good Service".
- **Worst Day Analysis**: Analysis of which day of the week has the most incidents.
- **Worst Hour Analysis**: Hourly breakdown of disruption frequency.

### 5. Data Export

Dump the entire database to a JSON file for external use.

```bash
stack run -- dumpdata
```

---

## 🏗️ Technical Architecture

### Modules

- **`Fetch`**: Handles HTTP requests to TfL using `http-conduit`.
- **`Parse`**: Complex JSON parsing using `aeson`. Handles nested geometry (polygons/bounds) for roads and disruptions.
- **`Database`**: SQLite interactions using `sqlite-simple`. Implements safe, parameterized queries.
- **`Actions`**: Contains business logic for the interactive search flows (`Search.hs` and submodules).
- **`Utils`**: Helper functions for Environment variables and Terminal display formatting.

### Database Schema

- **`roads`**: Stores static data (ID, Name, Geometry).
- **`road_status_logs`**: Time-series data storing status updates for trend analysis.
- **`road_disruptions`**: Stores detailed disruption info including polygon geometry for geospatial calculations.

### Extra Features Implemented

1.  **Traffic Trend Analyzer**: Aggregates historical logs to find temporal patterns (Worst Day/Hour).
2.  **Geospatial Recommendation Engine**: Uses the Haversine formula and bounding-box logic to map coordinates to roads, providing actionable travel advice ("Best Option").

---

## 🔗 Credits & References

- **Source**: [TfL Unified API](https://api.tfl.gov.uk/)
- **Course**: ECS713U/P Functional Programming
- **University**: Queen Mary University of London