module Day3 where

import qualified Data.List as L
import qualified Data.Char as C

type EngineSchematic = [Text]

-- partNumbers :: EngineSchematic -> [Int]
-- partNumbers eng = _pn

data Coords = Coords
  { row :: Int
  , col :: Int
  } deriving (Eq, Show)

-- allNumbers :: EngineSchematic -> [([Coords], Int)]
-- allNumbers eng = _an

-- allSymbols :: EngineSchematic -> [(Coords, Char)]
-- -- concat
-- -- findIndices
-- allSymbols eng = _as

findWithIndices :: (a -> Bool) -> [a] -> [(Int, a)]
findWithIndices p = filter (p . snd) . zip [0 ..]

rowNumbers
  :: Int -- ^ Row number
  -> Text -- ^ Line (row) of text
  -> [([Coords], Int)] -- ^ Each found number along with all of the coordinates it touches
rowNumbers r =
  map (\g -> (map (\(c, _) -> Coords r c) g, fromMaybe 0 . readMaybe . map snd $ g))
  . filter (all $ C.isDigit . snd)
  . L.groupBy (\(_, a) (_, b) -> C.isDigit a && C.isDigit b)
  . zip ([0 ..] :: [Int])
  . toString

rowSymbols
  :: Int -- ^ Row number
  -> Text -- ^ Line (row) of text
  -> [(Coords, Char)] -- ^ Each symbol and their coordinates
rowSymbols r =
  map (\(c, x) -> (Coords r c, x))
  . findWithIndices (\x -> not (x `elem` (".1234567890" :: [Char])))
  . toString

adjacent :: Coords -> Coords -> Bool
adjacent a b = abs (row a - row b) <= 1 && abs (col a - col b) <= 1
