module Day1 where

import qualified Data.List as L
import qualified Relude.Extra.Map as Map

extractCalibrationValue :: Text -> Int
extractCalibrationValue x = (firstDigit * 10) + secondDigit
  where
    digits :: NonEmpty Int
    digits = fromMaybe (0 :| []) $
      nonEmpty . catMaybes . map (readMaybe . pure) . toString $ x
    
    firstDigit = head digits
    
    secondDigit = last digits

digitWords :: [(String, Int)]
digitWords =
  [ ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  , ("1", 1)
  , ("2", 2)
  , ("3", 3)
  , ("4", 4)
  , ("5", 5)
  , ("6", 6)
  , ("7", 7)
  , ("8", 8)
  , ("9", 9)
  ]

subIndices :: Eq a => [a] -> [a] -> [Int]
subIndices needle haystack = L.findIndices (needle `isPrefixOf`) (tails haystack)

-- This needs to store *all* the indexes of each needle, not just the first one.
findDigits :: Text -> Map Int Int -- index:digit
findDigits t = foldr
  (\(needle, digit) m -> foldr (\i -> Map.insert i digit) m $ subIndices needle (toString t))
  mempty
  digitWords

extractCalibrationValueWithWords :: Text -> Int
extractCalibrationValueWithWords t = (firstDigit * 10) + secondDigit
  where
    firstDigit = snd . head $ digitPairs

    secondDigit = snd . last $ digitPairs

    digitPairs :: NonEmpty (Int, Int)
    digitPairs = fromMaybe ((0, 0) :| [])
      . nonEmpty
      . sortOn fst
      . Map.toPairs
      . findDigits
      $ t
    
part1 :: IO ()
part1 = do
  t :: Text <- decodeUtf8With lenientDecode <$> readFileBS "input/Day1.txt"
  print $ sum . map extractCalibrationValue . lines $ t

part2 :: IO ()
part2 = do
  t :: Text <- decodeUtf8With lenientDecode <$> readFileBS "input/Day1.txt"
  print $ sum . map extractCalibrationValueWithWords . lines $ t
