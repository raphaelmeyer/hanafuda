module CardSpec where

import qualified Cards
import qualified Data.List as List
import Test.Hspec

spec :: Spec
spec = do
  describe "deck" $ do
    it "contains 48 cards" $ do
      length Cards.deck `shouldBe` 48

    it "each card should be uniquely identifiable" $ do
      List.nub Cards.deck `shouldMatchList` Cards.deck

    it "contains five hikari cards" $ do
      let hikari = Cards.withRank Cards.Hikari Cards.deck
      length hikari `shouldBe` 5

    it "contains nine tane cards" $ do
      let tane = Cards.withRank Cards.Tane Cards.deck
      length tane `shouldBe` 9

    describe "tanzaku cards" $ do
      it "contains three akatan cards" $ do
        let akatan = Cards.withRank Cards.Akatan Cards.deck
        length akatan `shouldBe` 3

      it "contains three aotan cards" $ do
        let aotan = Cards.withRank Cards.Aotan Cards.deck
        length aotan `shouldBe` 3

      it "contains four tanzaku cards" $ do
        let tanzaku = Cards.withRank Cards.Tanzaku Cards.deck
        length tanzaku `shouldBe` 4

    it "contains 24 kasu cards" $ do
      let kasu = Cards.withRank Cards.Kasu Cards.deck
      length kasu `shouldBe` 24

  describe "card" $ do
    it "may have a name" $ do
      Cards.fromName Cards.Houou `shouldSatisfy` Cards.hasName Cards.Houou

      let ino = head . Cards.withRank Cards.Tane . Cards.withSuit Cards.Kiku $ Cards.deck
      ino `shouldNotSatisfy` Cards.hasName Cards.Yamajishi
