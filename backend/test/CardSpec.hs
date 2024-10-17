module CardSpec where

import qualified Cards
import qualified Data.List as List
import Test.Hspec

spec :: Spec
spec = do
  describe "deck" $ do
    it "contains 48 cards" $ do
      length Cards.deck `shouldBe` 48

    it "contains five hikari cards" $ do
      let hikari = filter ((== Cards.Hikari) . Cards.points) Cards.deck
      length hikari `shouldBe` 5

    it "contains nine tane cards" $ do
      let tane = filter ((== Cards.Tane) . Cards.points) Cards.deck
      length tane `shouldBe` 9

    describe "tan cards" $ do
      let tanCards = filter ((== Cards.Tan) . Cards.points) Cards.deck

      it "contains three akatan cards" $ do
        let akatan = filter ((== Cards.Akatan) . Cards.name) tanCards
        length akatan `shouldBe` 3

      it "contains three aotan cards" $ do
        let aotan = filter ((== Cards.Aotan) . Cards.name) tanCards
        length aotan `shouldBe` 3

      it "contains four tanzaku cards" $ do
        let tanzaku = filter ((== Cards.Tanzaku) . Cards.name) tanCards
        length tanzaku `shouldBe` 4

    it "each card should be uniquely identifiable" $ do
      List.nub Cards.deck `shouldMatchList` Cards.deck
