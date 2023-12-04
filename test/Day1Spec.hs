module Day1Spec (spec) where

import Test.Hspec
import qualified Day1

spec :: Spec
spec = do
  describe "extractCalibrationValue" $ do
    it "works on basic examples" $ do
      Day1.extractCalibrationValue "1abc2" `shouldBe` 12
      Day1.extractCalibrationValue "pqr3stu8vwx" `shouldBe` 38
      Day1.extractCalibrationValue "a1b2c3d4e5f" `shouldBe` 15
      Day1.extractCalibrationValue "treb7uchet" `shouldBe` 77

  describe "extractCalibrationValueWithWords" $ do
    it "works on basic examples" $ do
      Day1.extractCalibrationValueWithWords "two1nine" `shouldBe` 29
      Day1.extractCalibrationValueWithWords "eightwothree" `shouldBe` 83
      Day1.extractCalibrationValueWithWords "abcone2threexyz" `shouldBe` 13
      Day1.extractCalibrationValueWithWords "xtwone3four" `shouldBe` 24
      Day1.extractCalibrationValueWithWords "4nineeightseven2" `shouldBe` 42
      Day1.extractCalibrationValueWithWords "zoneight234" `shouldBe` 14
      Day1.extractCalibrationValueWithWords "7pqrstsixteen" `shouldBe` 76

    it "works on tricky examples" $ do
      Day1.extractCalibrationValueWithWords "twone" `shouldBe` 21
      Day1.extractCalibrationValueWithWords "oneight" `shouldBe` 18
      Day1.extractCalibrationValueWithWords "seightwoone8qxcfgszninesvfcnxc68" `shouldBe` 88
