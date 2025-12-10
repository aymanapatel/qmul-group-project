# TfL Road Flow Tracker Report

## Application Overview

The **TfL Road Flow Tracker** is a Haskell application designed to harvest, store, and analyze road status data from the Transport for London (TfL) API. It allows users to build a historical database of road disruptions and perform trend analysis to identify reliability issues.

## Compilation and Usage

### Prerequisites

- Haskell Stack
- TfL API Key (stored in `.env`)

### Compilation

To build the project, run:

```bash
stack build
```

### Usage

The application is run via `stack run` with specific commands:

1.  **Initialize Database**:

    ```bash
    stack run -- create
    ```

    Creates `tfl.db` with `roads` and `road_status_logs` tables.

2.  **Load Data**:

    ```bash
    stack run -- loaddata
    ```

    Fetches current road status from the TfL API and saves it to the database. Run this periodically to build history.

3.  **Generate Report**:

    ```bash
    stack run -- report
    ```

    Displays a **Reliability Report** (percentage of "Good Service") and a **Traffic Trend Analysis** (worst days/hours for disruptions).

4.  **Dump Data**:
    ```bash
    stack run -- dumpdata
    ```
    Exports all database logs to `data.json`.

## Web Source and Extraction

- **Source**: [TfL Unified API - Road Endpoint](https://api.tfl.gov.uk/Road)
- **Extraction**: The app uses `http-conduit` to send a GET request to the API. The JSON response is parsed using `aeson` into Haskell data types (`Road`).
- **Authentication**: The API key is securely loaded from a `.env` file and appended to the request URL.

## Design Choices

- **Database Schema**: A normalized schema with `roads` (static data) and `road_status_logs` (time-series data) ensures efficient storage and allows for complex historical queries.
- **Environment Variables**: Storing the API key in `.env` prevents hardcoding sensitive credentials.
- **Modular Structure**: Code is separated into `Fetch`, `Parse`, `Database`, and `Types` modules for maintainability and separation of concerns.

## Extra Feature: Traffic Trend Analyzer

Beyond the basic requirements, we implemented a **Traffic Trend Analyzer**.

- **Functionality**: This feature analyzes the historical data in `road_status_logs` to identify patterns in traffic disruptions.
- **Implementation**: It uses SQL aggregation queries (`GROUP BY`, `COUNT`, `strftime`) to calculate:
  - **Worst Day of the Week**: Which days have the most disruptions.
  - **Worst Hour of the Day**: What times are most prone to traffic issues.
- **Technical Complexity**: This required advanced SQL usage within Haskell and logic to map numerical day representations (0-6) to human-readable names (Sunday-Saturday).
