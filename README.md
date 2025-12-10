# TfL Road Flow Tracker

## Group Coursework — ECS713U/P Functional Programming 2025/26

This project is a **stack-based Haskell app** for harvesting information from the Transport for London (TfL) API and saving it to a database. It allows users to query the data and analyze traffic trends.

## Application Overview

The **TfL Road Flow Tracker** is designed to harvest, store, and analyze road status data. It allows users to build a historical database of road disruptions and perform trend analysis to identify reliability issues.

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

4.  **Generate Report**:

    ```bash
    stack run -- report
    ```

    Displays a **Reliability Report** (percentage of "Good Service") and a **Traffic Trend Analysis** (worst days/hours for disruptions).

5.  **Interactive Search**:

    ```bash
    stack run -- search
    ```

    Allows you to search for roads by name (partial match) and view their latest status.

6.  **Dump Data**:
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

---

## Original Project Specification (Reference)

### Building project guidelines

1. Run build command

```shell
stack build
```

2. Run

```shell
stack exec tfl-app
```

### Instructions

- Remove `.stack-work` before putting the zip
- Do not touch `tfl-app.cabal`

[Project specification](https://learnouts.com/student/72/cw/121/)

**This is a group project:**

- Recommended: Use `git` for collaboration and version tracking
- All students have accounts on QM GitHub: [https://github.qmul.ac.uk](https://github.qmul.ac.uk)

---

## **Task Overview**

Your group will implement a **stack-based Haskell app** for harvesting information from the Web and saving it to a database.

- Use `stack` to create, build, and run your project.
- Name your stack project: **haskell-project**
- The app must enable users to query the database.

### **Main Tasks**

1. Your group will be assigned one **specific web API (JSON)** containing data of interest.
2. **Module structure:**
   - `Types.hs`: Defines Haskell data types
   - `Fetch.hs`: Function for downloading JSON docs from the web API
   - `Parse.hs`: Parses downloaded data into your Haskell datatype
3. **Database module:**
   - `Database.hs`: Creates DB tables, saves/retrieves data using Haskell types
4. **JSON generation:**
   - Parsing module (`Parse.hs`) must generate a JSON representation and write to a file
5. **Main function (`Main.hs`):**
   | Command | Action |
   |---------|--------|
   | stack run -- create | Creates sqlite database and tables |
   | stack run -- loaddata | Downloads data from API and saves to database |
   | stack run -- dumpdata | Generates `data.json` with all DB data |
   | stack run -- | Run queries on the DB (your choice) |
6. **Code comments:** Use haddock notation for automatic documentation generation
7. **Extra feature:** Implement an additional challenging feature to demonstrate technical ability (required for full marks)
8. **Report:** Write a **1-2 page(s) report** explaining your app, how to run it, and justify any design choices. Describe and justify any extra features implemented.

---

## **Web API Assignment**

Your group should use one of these APIs based on your group number:

| Web API              | URL                                 | Condition                 |
| -------------------- | ----------------------------------- | ------------------------- |
| Transport for London | [API Link](https://api.tfl.gov.uk/) | group_number `mod` 7 == 6 |

---

## **Required Haskell Modules**

- Use dependencies managed via `stack`, e.g., [lts-24-18](https://www.stackage.org/lts-24.18)
- **Database access:** [sqlite-simple](https://www.stackage.org/lts-24.18/package/sqlite-simple)
- **HTTP requests:** [http-conduit](https://www.stackage.org/lts-24.18/package/http-conduit)
- **JSON parsing:** [aeson](https://www.stackage.org/lts-24.18/package/aeson)
- Must use `sqlite` (do not use MySQL or Postgres)

---

## **Report Requirements**

- Must be 1-2 pages in **PDF form** at the project top level
- Explain how to compile/run/use your app
- Detail your web source and extraction method
- Describe any extra features and their technical complexity

---

## **Marking Criteria**

| Criteria                                                           | Max. Mark |
| ------------------------------------------------------------------ | --------- |
| Project compiles and runs successfully                             | 10        |
| Basic functionality (download, parse, save on DB, queries) working | 15        |
| Project code structured into modules                               | 5         |
| Good knowledge of Haskell demonstrated                             | 10        |
| Haddock-style documentation included                               | 10        |
| Database: at least 2 tables, with relationship (foreign key)       | 15        |
| HTTP fetch function and error handling complexity                  | 8         |
| Data parsing complexity                                            | 7         |
| Report quality and coverage                                        | 10        |
| Complexity of extra work                                           | 10        |
| Adjustments                                                        | 0         |
| **Total:**                                                         | **100**   |
