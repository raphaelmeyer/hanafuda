module CardSpec where

import qualified Cards
import Test.Hspec

spec :: Spec
spec = do
  describe "deck" $ do
    it "contains 48 cards" $ do
      length Cards.deck `shouldBe` 48

    it "contains five hikari cards" $ do
      let hikari = filter ((== Cards.Hikari) . Cards.suit) Cards.deck
      length hikari `shouldBe` 5

    it "contains nine tane cards" $ do
      let tane = filter ((== Cards.Tane) . Cards.suit) Cards.deck
      length tane `shouldBe` 9

    describe "tan cards" $ do
      let tanCards = filter ((== Cards.Tan) . Cards.suit) Cards.deck

      it "contains three akatan cards" $ do
        let akatan = filter ((== Cards.Akatan) . Cards.rank) tanCards
        length akatan `shouldBe` 3

      it "contains three aotan cards" $ do
        let aotan = filter ((== Cards.Aotan) . Cards.rank) tanCards
        length aotan `shouldBe` 3

      it "contains four tanzaku cards" $ do
        let tanzaku = filter ((== Cards.Tanzaku) . Cards.rank) tanCards
        length tanzaku `shouldBe` 4
