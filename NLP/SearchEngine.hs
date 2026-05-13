module SearchEngine where

import Data.List (sortOn, nub)
import Data.Ord (Down(..))
import Data.Char (toLower, isAlphaNum)
import qualified Data.Map as Map

-- Datatypes
type DocumentId = String
type Document = (DocumentId, String)
type Index = Map.Map String [DocumentId]

data SearchResult = SearchResult {
    resDocId :: DocumentId,
    resScore :: Double
} deriving (Show, Eq)

-- Split text into tokens
tokenize :: String -> [String]
tokenize text = words text

-- Normalize
normalize :: [String] -> [String]
normalize tokens = filter (not . null) $ map clean tokens
  where clean = map toLower . filter isAlphaNum

-- Remove common stopwords
removeStopwords :: [String] -> [String]
removeStopwords = filter (`notElem` stopwords)
  where stopwords = ["the", "and", "is", "in", "to", "with", "a", "an", "of", "it", "for", "on", "as", "at", "by"]


processText :: String -> [String]
processText = removeStopwords . normalize . tokenize

-- Put words into an inverted index
buildIndex :: [Document] -> Index
buildIndex docs = foldl insertDoc Map.empty docs
  where
    insertDoc idx (docId, text) =
        let terms = nub (processText text) 
        in foldl (\acc term -> Map.insertWith (++) term [docId] acc) idx terms

-- Count term frequencies for scoring
search :: Index -> String -> [SearchResult]
search idx query =
    let queryTerms = processText query
        docHits = concatMap (\term -> Map.findWithDefault [] term idx) queryTerms
        docScores = Map.fromListWith (+) [(docId, 1.0) | docId <- docHits]
        results = [SearchResult docId score | (docId, score) <- Map.toList docScores]
    in sortOn (Down . resScore) results