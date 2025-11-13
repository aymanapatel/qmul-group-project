
# Group Coursework — ECS713U/P Functional Programming 2025/26


## Building project guidelines

1. Run build command

```shell
stack build
```

2. Run 

```shell
stack exec tfl-app
```

## Instructions

- Remove `.stack-work` before putting the zip
- Do not touch `tfl-app.cabal`

[Project specification](https://learnouts.com/student/72/cw/121/)


Here is the content of your LearnOuts.com group coursework page formatted in **Markdown** for clarity and ease of use:



**This is a group project:**  
- Recommended: Use `git` for collaboration and version tracking  
- All students have accounts on QM GitHub: [https://github.qmul.ac.uk](https://github.qmul.ac.uk)

***

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

***

## **Web API Assignment**

Your group should use one of these APIs based on your group number:

| Web API                 | URL                                                        | Condition                       |
|-------------------------|------------------------------------------------------------|----------------------------------|
| Transport for London    | [API Link](https://api.tfl.gov.uk/)                        | group_number `mod` 7 == 6        |

***

## **Required Haskell Modules**

- Use dependencies managed via `stack`, e.g., [lts-24-18](https://www.stackage.org/lts-24.18)
- **Database access:** [sqlite-simple](https://www.stackage.org/lts-24.18/package/sqlite-simple)
- **HTTP requests:** [http-conduit](https://www.stackage.org/lts-24.18/package/http-conduit)
- **JSON parsing:** [aeson](https://www.stackage.org/lts-24.18/package/aeson)
- Must use `sqlite` (do not use MySQL or Postgres)

***

## **Report Requirements**

- Must be 1-2 pages in **PDF form** at the project top level
- Explain how to compile/run/use your app
- Detail your web source and extraction method
- Describe any extra features and their technical complexity

***

## **Marking Criteria**

| Criteria                                                            | Max. Mark                       |
|---------------------------------------------------------------------|---------------------------------|
| Project compiles and runs successfully                              | 10                              |
| Basic functionality (download, parse, save on DB, queries) working  | 15                              |
| Project code structured into modules                                | 5                               |
| Good knowledge of Haskell demonstrated                              | 10                              |
| Haddock-style documentation included                                | 10                              |
| Database: at least 2 tables, with relationship (foreign key)        | 15                              |
| HTTP fetch function and error handling complexity                   | 8                               |
| Data parsing complexity                                             | 7                               |
| Report quality and coverage                                         | 10                              |
| Complexity of extra work                                            | 10                              |
| Adjustments                                                         | 0                               |
| **Total:**                                                          | **100**                         |

***

© 2020 - 2025 Oliva Apps Ltd

***

Let me know if you want a more specific section, code block, or table from the above!

[1](https://learnouts.com/student/72/cw/121/)


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