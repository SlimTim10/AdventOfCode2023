module Day1 where

extractCalibrationValue :: Text -> Int
extractCalibrationValue x = (firstDigit * 10) + secondDigit
  where
    digits :: NonEmpty Int
    digits = fromMaybe (0 :| []) $
      nonEmpty . catMaybes . map (readMaybe . pure) . toString $ x
    
    firstDigit = head digits
    
    secondDigit = last digits

part1 :: IO ()
part1 = do
  t :: Text <- decodeUtf8With lenientDecode <$> readFileBS "input/Day1.txt"
  print $ sum . map extractCalibrationValue . lines $ t
