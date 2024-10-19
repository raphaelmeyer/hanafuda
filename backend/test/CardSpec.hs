module CardSpec where

import qualified Cards
import Control.Exception (evaluate)
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
      let hikari = Cards.withPoints Cards.Hikari Cards.deck
      length hikari `shouldBe` 5

    it "contains nine tane cards" $ do
      let tane = Cards.withPoints Cards.Tane Cards.deck
      length tane `shouldBe` 9

    describe "tan cards" $ do
      let tanCards = Cards.withPoints Cards.Tan Cards.deck

      it "contains ten tan cards" $ do
        length tanCards `shouldBe` 10

      it "contains three akatan cards" $ do
        let akatan = Cards.withName Cards.Akatan tanCards
        length akatan `shouldBe` 3

      it "contains three aotan cards" $ do
        let aotan = Cards.withName Cards.Aotan tanCards
        length aotan `shouldBe` 3

      it "contains four tanzaku cards" $ do
        let tanzaku = Cards.withName Cards.Tanzaku tanCards
        length tanzaku `shouldBe` 4

    it "contains 24 kasu cards" $ do
      let kasu = Cards.withPoints Cards.Kasu Cards.deck
      length kasu `shouldBe` 24

  describe "card" $ do
    it "throws if card is invalid" $ do
      evaluate (Cards.makeCard Cards.Fuji Cards.Tane Cards.Chou) `shouldThrow` errorCall "invalid card"

    it "has points category" $ do
      let momijiAotan = Cards.makeCard Cards.Momiji Cards.Tan Cards.Aotan
      let yanagiTsubame = Cards.makeCard Cards.Yanagi Cards.Tane Cards.Tsubame

      momijiAotan `shouldSatisfy` Cards.hasPoints Cards.Tan
      momijiAotan `shouldNotSatisfy` Cards.hasPoints Cards.Hikari
      momijiAotan `shouldNotSatisfy` Cards.hasPoints Cards.Tane
      momijiAotan `shouldNotSatisfy` Cards.hasPoints Cards.Kasu

      yanagiTsubame `shouldSatisfy` Cards.hasPoints Cards.Tane
      yanagiTsubame `shouldNotSatisfy` Cards.hasPoints Cards.Hikari
      yanagiTsubame `shouldNotSatisfy` Cards.hasPoints Cards.Tan
      yanagiTsubame `shouldNotSatisfy` Cards.hasPoints Cards.Kasu

    it "has a name" $ do
      let houou = Cards.makeCard Cards.Kiri Cards.Hikari Cards.Houou
      let umeAkatan = Cards.makeCard Cards.Ume Cards.Tan Cards.Akatan

      houou `shouldSatisfy` Cards.hasName Cards.Houou
      houou `shouldNotSatisfy` Cards.hasName Cards.Mankai

      umeAkatan `shouldSatisfy` Cards.hasName Cards.Akatan
      umeAkatan `shouldNotSatisfy` Cards.hasName Cards.Tanzaku
      umeAkatan `shouldNotSatisfy` Cards.hasName Cards.Aotan

  describe "withPoints" $ do
    it "should return only cards with these points" $ do
      let hikari = Cards.withPoints Cards.Hikari Cards.deck
      hikari `shouldContain` [Cards.makeCard Cards.Susuki Cards.Hikari Cards.Mochiduki]
      hikari `shouldSatisfy` all (Cards.hasPoints Cards.Hikari)

      let tanzaku = Cards.withPoints Cards.Tan Cards.deck
      tanzaku `shouldContain` [Cards.makeCard Cards.Hagi Cards.Tan Cards.Tanzaku]
      tanzaku `shouldSatisfy` all (Cards.hasPoints Cards.Tan)
