module SpamClassifier where

import qualified Data.Map as Map
import Data.List (isPrefixOf)

-- Algebraic datatypes
data Label = Spam | Ham deriving (Show, Eq)

-- Records: Our trained model
data Model = Model {
    spamCounts :: Map.Map String Int,
    hamCounts  :: Map.Map String Int,
    spamProb   :: Double,
    hamProb    :: Double,
    totalSpamWords :: Int,
    totalHamWords  :: Int,
    vocabSize  :: Int
} deriving (Show)

type Document = (Label, [String])

-- 1. parseLine (Uses Maybe for safe parsing)
parseLine :: String -> Maybe (Label, String)
parseLine line
    | "ham\t" `isPrefixOf` line  = Just (Ham, drop 4 line)
    | "spam\t" `isPrefixOf` line = Just (Spam, drop 5 line)
    | otherwise                  = Nothing

-- 2. train (Uses foldl and Map for word counts)
train :: [Document] -> Model
train docs = Model sCounts hCounts pSpam pHam sTotal hTotal vSize
  where
    totalDocs = length docs
    spamDocs  = [words | (Spam, words) <- docs]
    hamDocs   = [words | (Ham, words) <- docs]

    pSpam = fromIntegral (length spamDocs) / fromIntegral totalDocs
    pHam  = fromIntegral (length hamDocs)  / fromIntegral totalDocs

    -- Counts the frequencies of the words
    buildFreqMap :: [[String]] -> Map.Map String Int
    buildFreqMap docList = foldl addWords Map.empty docList
      where
        addWords acc wordsList = foldl (\m w -> Map.insertWith (+) w 1 m) acc wordsList

    sCounts = buildFreqMap spamDocs
    hCounts = buildFreqMap hamDocs

    sTotal = sum (Map.elems sCounts)
    hTotal = sum (Map.elems hCounts)
    vSize  = Map.size (Map.union sCounts hCounts)

-- 3. predict (Classification with Laplace smoothing and log probabilities)
predict :: Model -> [String] -> Label
predict model words = if spamScore > hamScore then Spam else Ham
  where
    spamScore = log (spamProb model) + sum (map (logProb (spamCounts model) (totalSpamWords model)) words)
    hamScore  = log (hamProb model)  + sum (map (logProb (hamCounts model) (totalHamWords model)) words)

    logProb counts total w =
        let count = fromIntegral $ Map.findWithDefault 0 w counts
            -- Laplace smoothing (Avoids probability 0 for unknown words)
            num   = count + 1.0
            den   = fromIntegral total + fromIntegral (vocabSize model)
        in log (num / den)

-- 4. accuracy (Evaluation)
accuracy :: Model -> [Document] -> Double
accuracy model testData =
    let correct = filter (\(label, words) -> predict model words == label) testData
    in fromIntegral (length correct) / fromIntegral (length testData)