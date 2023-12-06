module Day3Spec (spec) where

import Test.Hspec
import qualified Day3

spec :: Spec
spec = do
  describe "adjacent" $ do
    it "returns true for coordinates that are the same" $ do
      Day3.adjacent
        Day3.Coords { row = 1, col = 1 }
        Day3.Coords { row = 1, col = 1 }
        `shouldBe` True

    it "returns true for coordinates that are vertically adjacent" $ do
      Day3.adjacent
        Day3.Coords { row = 1, col = 1 }
        Day3.Coords { row = 2, col = 1 }
        `shouldBe` True
      Day3.adjacent
        Day3.Coords { row = 2, col = 1 }
        Day3.Coords { row = 1, col = 1 }
        `shouldBe` True

    it "returns true for coordinates that are horizontally adjacent" $ do
      Day3.adjacent
        Day3.Coords { row = 1, col = 1 }
        Day3.Coords { row = 1, col = 2 }
        `shouldBe` True
      Day3.adjacent
        Day3.Coords { row = 2, col = 2 }
        Day3.Coords { row = 1, col = 1 }
        `shouldBe` True

    it "returns true for coordinates that are diagonally adjacent" $ do
      Day3.adjacent
        Day3.Coords { row = 1, col = 1 }
        Day3.Coords { row = 2, col = 2 }
        `shouldBe` True
      Day3.adjacent
        Day3.Coords { row = 5, col = 5 }
        Day3.Coords { row = 4, col = 4 }
        `shouldBe` True
      Day3.adjacent
        Day3.Coords { row = 5, col = 5 }
        Day3.Coords { row = 4, col = 6 }
        `shouldBe` True
      Day3.adjacent
        Day3.Coords { row = 5, col = 5 }
        Day3.Coords { row = 6, col = 4 }
        `shouldBe` True

    it "returns false for coordinates that are not adjacent" $ do
      Day3.adjacent
        Day3.Coords { row = 1, col = 1 }
        Day3.Coords { row = 1, col = 3 }
        `shouldBe` False
      Day3.adjacent
        Day3.Coords { row = 1, col = 1 }
        Day3.Coords { row = 3, col = 1 }
        `shouldBe` False
      Day3.adjacent
        Day3.Coords { row = 3, col = 1 }
        Day3.Coords { row = 1, col = 1 }
        `shouldBe` False
      Day3.adjacent
        Day3.Coords { row = 1, col = 3 }
        Day3.Coords { row = 1, col = 1 }
        `shouldBe` False

  describe "rowSymbols" $ do
    it "gets the right row symbols" $ do
      Day3.rowSymbols 0 "...*......"
        `shouldBe`
        [ (Day3.Coords 0 3, '*')
        ]
      Day3.rowSymbols 0 "...$.*...."
        `shouldBe`
        [ (Day3.Coords 0 3, '$')
        , (Day3.Coords 0 5, '*')
        ]
      Day3.rowSymbols 0 "617*......"
        `shouldBe`
        [ (Day3.Coords 0 3, '*')
        ]

  describe "rowNumbers" $ do
    it "gets the right row numbers" $ do
      Day3.rowNumbers 0 "467..114.."
        `shouldBe`
        [ ([Day3.Coords 0 0, Day3.Coords 0 1, Day3.Coords 0 2], 467)
        , ([Day3.Coords 0 5, Day3.Coords 0 6, Day3.Coords 0 7], 114)
        ]
      Day3.rowNumbers 0 "617*......"
        `shouldBe`
        [ ([Day3.Coords 0 0, Day3.Coords 0 1, Day3.Coords 0 2], 617)
        ]
      Day3.rowNumbers 0 "...$.*...."
        `shouldBe`
        []

  -- describe "partNumbers" $ do
  --   it "gets the right part numbers for the given example" $ do
  --     let engineSchematic = unlines
  --           [ "467..114.."
  --           , "...*......"
  --           , "..35..633."
  --           , "......#..."
  --           , "617*......"
  --           , ".....+.58."
  --           , "..592....."
  --           , "......755."
  --           , "...$.*...."
  --           , ".664.598.."
  --           ]
  --     Day3.partNumbers engineSchematic `shouldBe` [467, 35, 633, 617, 592, 755, 664, 598]

