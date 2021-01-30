{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  domainSpec

domainSpec :: Spec
domainSpec = do
  describe "test" $ do
    it "1 should equal 1" $ do
      1 `shouldBe` 1