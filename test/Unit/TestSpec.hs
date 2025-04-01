-- Unit test for unit tests
-- Sanity check for if tests are working
module Unit.TestSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
    describe "1+1" $ do
        it "equals 2" $ do
            1+1 `shouldBe` 2