module Day2Spec (spec) where

import Test.Hspec
import qualified Day2

spec :: Spec
spec = do
  describe "Day2" $ do
    it "parseGame parses games" $ do
      Day2.parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" `shouldBe`
        Day2.Game
        { gameId = 1
        , revelations =
          [ fromList [(Day2.Blue, 3), (Day2.Red, 4)]
          , fromList [(Day2.Red, 1), (Day2.Green, 2), (Day2.Blue, 6)]
          , fromList [(Day2.Green, 2)]
          ]
        }
      Day2.parseGame "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue" `shouldBe`
        Day2.Game
        { gameId = 2
        , revelations =
          [ fromList [(Day2.Blue, 1), (Day2.Green, 2)]
          , fromList [(Day2.Green, 3), (Day2.Blue, 4), (Day2.Red, 1)]
          , fromList [(Day2.Green, 1), (Day2.Blue, 1)]
          ]
        }
      Day2.parseGame "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red" `shouldBe`
        Day2.Game
        { gameId = 3
        , revelations =
          [ fromList [(Day2.Green, 8), (Day2.Blue, 6), (Day2.Red, 20)]
          , fromList [(Day2.Blue, 5), (Day2.Red, 4), (Day2.Green, 13)]
          , fromList [(Day2.Green, 5), (Day2.Red, 1)]
          ]
        }
      Day2.parseGame "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red" `shouldBe`
        Day2.Game
        { gameId = 4
        , revelations =
          [ fromList [(Day2.Green, 1), (Day2.Red, 3), (Day2.Blue, 6)]
          , fromList [(Day2.Green, 3), (Day2.Red, 6)]
          , fromList [(Day2.Green, 3), (Day2.Blue, 15), (Day2.Red, 14)]
          ]
        }
      Day2.parseGame "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" `shouldBe`
        Day2.Game
        { gameId = 5
        , revelations =
          [ fromList [(Day2.Red, 6), (Day2.Blue, 1), (Day2.Green, 3)]
          , fromList [(Day2.Blue, 2), (Day2.Red, 1), (Day2.Green, 2)]
          ]
        }
