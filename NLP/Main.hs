module Main where

import SearchEngine (Document, buildIndex, search, resDocId, resScore, processText)
import SpamClassifier (parseLine, train, predict, accuracy, Model)
import Data.Maybe (mapMaybe)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "=== 1. Search Engine Demo ==="
    
    let documents = [
            ("doc1", "Haskell is a functional programming language."),
            ("doc2", "Machine learning and NLP are very interesting."),
            ("doc3", "Python is also used for machine learning and NLP."),
            ("doc4", "Functional programming concepts include map, filter, and foldl.")
            ]

    putStrLn "Building inverted index from documents..."
    let index = buildIndex documents

    let queries = ["Haskell", "functional programming", "NLP Machine Learning", "Python"]

    mapM_ (\q -> do
        putStrLn $ "\nSearching for: \"" ++ q ++ "\""
        let results = search index q
        if null results
            then putStrLn "  -> No results found."
            else mapM_ (\res -> putStrLn $ "  -> Document: " ++ resDocId res ++ " (Score: " ++ show (resScore res) ++ ")") results
        ) queries

    putStrLn "\n=== 2. Spam Classifier Demo ==="
    putStrLn "Loading dataset SMSSpamCollection..."
    content <- readFile "SMSSpamCollection"
    
    -- The pure functional pipeline: parseLine -> processText
    let parsed = mapMaybe parseLine (lines content)
        processed = map (\(label, text) -> (label, processText text)) parsed
        
        -- Split into training and test data (e.g., 80/20)
        splitIdx = (length processed * 80) `div` 100
        (trainData, testData) = splitAt splitIdx processed

    putStrLn $ "Training model (" ++ show (length trainData) ++ " examples)..."
    let model = train trainData

    putStrLn $ "Testing model (" ++ show (length testData) ++ " examples)..."
    let acc = accuracy model testData
    putStrLn $ "Accuracy: " ++ show (acc * 100) ++ "%\n"

    putStrLn "--- Interactive Spam Detection ---"
    putStrLn "Type an SMS (or 'quit' to exit):"
    demoLoop model

-- Recursive function for the demo loop
demoLoop :: Model -> IO ()
demoLoop model = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if input == "quit"
        then putStrLn "Goodbye!"
        else do
            let prediction = predict model (processText input)
            putStrLn $ "Classification: " ++ show prediction
            demoLoop model