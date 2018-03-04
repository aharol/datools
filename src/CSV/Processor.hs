module LearningHaskellDataAnalysis02 where

import Data.List

import Data.Either
import Text.CSV
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Regex.Posix ((=~))

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / fromIntegral (length xs)


-- Opens a CSV file and applies a function to a column
-- Returns Either Error Message or the function result
applyToColumnInCSVFile :: ([String] -> b) -> FilePath -> String -> IO (Either String b)
applyToColumnInCSVFile func inFileName column = do
    -- Open and read CSV file
    input <- readFile inFileName
    let records = parseCSV inFileName input
    -- Check to make sure this is a good csv file
    return $ either
        handleCSVError
        (\csv -> applyToColumnInCSV func csv column) records
    where
        handleCSVError csv = Left "This does not appear to be a CSV file."

getColumnInCSV :: CSV -> String -> Either String Integer
getColumnInCSV csv columnName =
    case lookupResponse of
        Nothing -> Left
            "The column does not exist in the CSV file."
        Just x -> Right (fromIntegral x) -- converts from Int to Num
    where
        lookupResponse = findIndex (== columnName) (head csv)

applyToColumnInCSV :: ([String] -> b) -> CSV -> String -> Either String b
applyToColumnInCSV func csv column = either
    Left
    (Right . func . elements) columnIndex
    where
        columnIndex = getColumnInCSV csv column
        nfieldsInFile = length $ head csv
        records = tail $ filter (\record -> nfieldsInFile == length record) csv
        elements ci = map (\record -> genericIndex record ci) records

readColumn :: [String] -> [Double]
readColumn xs = map read xs

-- Converts a CSV expression into an SQL database
-- Returns "Successful" if successful, error message otherwise
convertCSVToSQL :: String -> FilePath -> [String] -> CSV -> IO ()
convertCSVToSQL tableName  outFileName fields records =
    -- Check to make sure that the number of columns matches the number of fields
    if nfieldsInFile == nfieldsInFields then do
        -- Open a connection
        conn <- connectSqlite3 outFileName

        -- Create a new table
        run conn createStatement []
        -- Load contents of CSv file into table
        stmt <- prepare conn insertStatement
        executeMany stmt (tail
            (filter (\record -> nfieldsInFile == length record)
            sqlRecords))

        -- Commit changes
        commit conn
        -- Close the connection
        disconnect conn
        -- Report that we were successful
        putStrLn "Successfull"
    else
        putStrLn "The number of input fields differ from the csv file."

    where
        nfieldsInFile = length $ head records
        nfieldsInFields = length fields
        createStatement = "CREATE TABLE " ++ tableName ++
                          " (" ++ (intercalate ", " fields) ++ ")"
        insertStatement = "INSERT INTO " ++ tableName ++
                          " VALUES (" ++ (intercalate ", " (replicate nfieldsInFile "?")) ++ ")"
        sqlRecords = map (\record -> map (\element -> toSql element) record) records

-- Converts a CSV file to an SQL database file
-- Prints "Successful" if successful, error message otherwise
convertCSVFileToSQL :: String -> String -> String -> [String] -> IO ()
convertCSVFileToSQL inFileName outFileName tableName fields = do
        -- Open and read the csv file
        input <- readFile inFileName
        let records = parseCSV inFileName input

        -- Check to make sure this is a good csv file
        either handleCSVError convertTool records
    where
        convertTool = convertCSVToSQL tableName outFileName fields
        handleCSVError csv = putStrLn "This does not apper to be a CSV file."

countFieldsInEachRecord :: CSV -> [Integer]
countFieldsInEachRecord = map genericLength

lineNumbersWithIncorrectCount :: CSV -> [(Integer, Integer)]
lineNumbersWithIncorrectCount (fields:csv) = filter
        (\(_, thisCount) -> thisCount /= nfields)
        lineNoCountPairs
    where
        nfields = genericLength fields
        count = countFieldsInEachRecord csv
        lineNoCountPairs = zip [1..] count

identifyMatchingFields :: (String -> Bool) -> [String] -> [String] -> Integer -> [(String, String, String)]
identifyMatchingFields myStringCmpFunc record headings idColumnIndex =
    filter (\(_,_,field) -> myStringCmpFunc field) keyvalue
    where
        nfields = length headings
        keyvalue = zip3 (replicate nfields (genericIndex record idColumnIndex)) headings record

identifyInCSV :: (String -> Bool) -> CSV -> String -> Either String [(String, String, String)]
identifyInCSV myFieldFunc csv idColumn = either
    Left
    (\ci -> Right $ concatMap (\rec -> identifyMatchingFields myFieldFunc rec (head csv) ci) (tail csv))
    columnIndex
    where
        headings = head csv
        columnIndex = getColumnInCSV csv idColumn

identifyInCSVFile :: (String -> Bool) -> String -> String -> IO (Either String [(String, String, String)])
identifyInCSVFile myStringCmpFunc inFileName idColumn = do
    records <- parseCSVFromFile inFileName
    return $ either
        (\err -> Left "This does not appear to be a CSV file")
        (\csv -> identifyInCSV myStringCmpFunc (init csv) idColumn)
        records

identifyInCSVFileFromColumn :: (String -> Bool) -> String -> String -> String -> IO (Either String [(String, String, String)])
identifyInCSVFileFromColumn myRegexFunc inFileName idColumn desiredHeading = do
    allFields <- identifyInCSVFile myRegexFunc inFileName idColumn
    return $ either
        Left
        (\af -> Right $ filter (\(_, heading, _) -> heading == desiredHeading) af)
        allFields
