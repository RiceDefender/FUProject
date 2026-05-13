module Main where

import SearchEngine (Document, buildIndex, search, resDocId, resScore)

-- Example documents for our mini search engine
documents :: [Document]
documents =
    [ ("doc1", "Haskell is a purely functional programming language. It uses map and fold.")
    , ("doc2", "Machine learning and NLP are exciting topics in modern computer science.")
    , ("doc3", "Functional programming uses map, filter, and fold for elegant solutions.")
    , ("doc4", "Implementing a small NLP project in Haskell is a very good idea for the FUP course.")
    ]

main :: IO ()
main = do
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