module KoiKoi.GameSpec where

import qualified Cards
import qualified KoiKoi.Game as Game
import Test.Hspec

spec :: Spec
spec = do
  describe "scoring" $ do
    describe "empty pile" $ do
      it "scores no points" $ do
        Game.score Game.emptyPile `shouldBe` 0

    describe "kasu" $ do
      let kasu = filter ((== Cards.Kasu) . Cards.points) Cards.deck

      it "scores no points for less than ten cards" $ do
        (Game.score . Game.makePile . take 1 $ kasu) `shouldBe` 0
        (Game.score . Game.makePile . take 7 $ kasu) `shouldBe` 0
        (Game.score . Game.makePile . take 9 $ kasu) `shouldBe` 0

      it "scores one point for ten cards" $ do
        (Game.score . Game.makePile . take 10 $ kasu) `shouldBe` 1

      it "scores on additional point for each additional card" $ do
        (Game.score . Game.makePile . take 11 $ kasu) `shouldBe` 2
        (Game.score . Game.makePile . take 14 $ kasu) `shouldBe` 5
