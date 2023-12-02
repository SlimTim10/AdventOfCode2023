module Day1Spec (spec) where

import Test.Hspec
import qualified Day1

spec :: Spec
spec = do
  describe "Day1" $ do
    it "extractCalibrationValue works" $ do
      Day1.extractCalibrationValue "1abc2" `shouldBe` 12
      Day1.extractCalibrationValue "pqr3stu8vwx" `shouldBe` 38
      Day1.extractCalibrationValue "a1b2c3d4e5f" `shouldBe` 15
      Day1.extractCalibrationValue "treb7uchet" `shouldBe` 77
