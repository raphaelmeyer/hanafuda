module CardSpec where

import qualified Cards
import Test.Hspec

spec :: Spec
spec = do
  describe "Deck" $ do
    it "contains 48 cards" $ do
      length Cards.deck `shouldBe` 48
