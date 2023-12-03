module Day2 where

import qualified Data.Text as T
import qualified Relude.Extra.Map as Map

data Game = Game
  { gameId :: Int
  , revelations :: [Revelation]
  } deriving (Show, Eq)

data Colour = Red | Green | Blue
  deriving (Eq, Ord, Show)

type Revelation = Map Colour Int

-- Same as Data.Text.breakOn, but don't keep the text matching the delimiter.
breakOnWithout :: Text -> Text -> (Text, Text)
breakOnWithout pat =
  (\(a, b) -> (a, T.drop (T.length pat) b))
  . T.breakOn pat

-- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
parseGame :: Text -> Game
parseGame t = Game
  { gameId = gameId'
  , revelations = revelations'
  }
  where
    (leftOfColon, rightOfColon) = fmap T.strip . breakOnWithout ":" $ t
    gameId' = fromMaybe 0 . readMaybe . toString . snd . breakOnWithout " " $ leftOfColon
    revelations' = parseRevelations rightOfColon

-- " 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
parseRevelations :: Text -> [Revelation]
parseRevelations = map parseRevelation . T.splitOn ";"

-- " 3 blue, 4 red"
parseRevelation :: Text -> Revelation
parseRevelation = foldr go mempty . map T.strip . T.splitOn ","
  where
    -- "3 blue"
    go :: Text -> Revelation -> Revelation
    go t m = fromMaybe m $ do
      let (leftPart, rightPart) = fmap T.strip . breakOnWithout " " $ t
      num <- readMaybe . toString $ leftPart
      colour <- parseColour rightPart
      return $ Map.insert colour num m

parseColour :: Text -> Maybe Colour
parseColour "red" = Just Red
parseColour "green" = Just Green
parseColour "blue" = Just Blue
parseColour _ = Nothing

validateGame :: Map Colour Int -> Game -> Bool
validateGame maxColours game = all validateRevelation (revelations game)
  where
    validateRevelation :: Revelation -> Bool
    validateRevelation r =
      all
      (\(colour, maxNum) -> Map.lookupDefault 0 colour r <= maxNum)
      $ Map.toPairs maxColours

part1 :: IO ()
part1 = do
  t :: Text <- decodeUtf8With lenientDecode <$> readFileBS "input/Day2.txt"
  let games = map parseGame . lines $ t
  let maxColours = fromList [(Red, 12), (Green, 13), (Blue, 14)] :: Map Colour Int
  let validGames = filter (validateGame maxColours) games
  print $ sum . map gameId $ validGames
