module KoiKoi.GameSpec where

import Cards
import qualified KoiKoi.Game as Game
import Test.Hspec

spec :: Spec
spec = do
  describe "scoring" $ do
    describe "empty pile" $ do
      it "scores no points" $ do
        Game.score Game.emptyPile `shouldBe` 0

    describe "kasu" $ do
      let kasu = withPoints Kasu deck

      it "scores no points for less than ten cards" $ do
        (Game.score . Game.makePile . take 1 $ kasu) `shouldBe` 0
        (Game.score . Game.makePile . take 7 $ kasu) `shouldBe` 0
        (Game.score . Game.makePile . take 9 $ kasu) `shouldBe` 0

      it "scores one point for ten cards" $ do
        (Game.score . Game.makePile . take 10 $ kasu) `shouldBe` 1

      it "scores one additional point for each additional card" $ do
        (Game.score . Game.makePile . take 11 $ kasu) `shouldBe` 2
        (Game.score . Game.makePile . take 14 $ kasu) `shouldBe` 5

    describe "hikari" $ do
      it "scores ten points for all five hikari cards" $ do
        let pile =
              Game.makePile
                [ makeCard Matsu Hikari Tsuru,
                  makeCard Sakura Hikari Mankai,
                  makeCard Susuki Hikari Mochiduki,
                  makeCard Yanagi Hikari Michikaze,
                  makeCard Kiri Hikari Houou
                ]
        Game.score pile `shouldBe` 10

      it "scores eight points for four hikari cards excluding Ono no Michikaze" $ do
        let pile =
              Game.makePile
                [ makeCard Matsu Hikari Tsuru,
                  makeCard Sakura Hikari Mankai,
                  makeCard Susuki Hikari Mochiduki,
                  makeCard Kiri Hikari Houou
                ]
        Game.score pile `shouldBe` 8

      it "scores seven points for four hikari cards including Ono no Michikaze" $ do
        let pile =
              Game.makePile
                [ makeCard Matsu Hikari Tsuru,
                  makeCard Susuki Hikari Mochiduki,
                  makeCard Yanagi Hikari Michikaze,
                  makeCard Kiri Hikari Houou
                ]
        Game.score pile `shouldBe` 7

      it "scores five points for three hikari cards excluding Ono no Michikaze" $ do
        let pile =
              Game.makePile
                [ makeCard Matsu Hikari Tsuru,
                  makeCard Sakura Hikari Mankai,
                  makeCard Kiri Hikari Houou
                ]
        Game.score pile `shouldBe` 5

      it "scores no points for other combinations" $ do
        let pile =
              Game.makePile
                [ makeCard Sakura Hikari Mankai,
                  makeCard Yanagi Hikari Michikaze,
                  makeCard Kiri Hikari Houou
                ]
        Game.score pile `shouldBe` 0

    describe "whole pile" $ do
      it "should add up all valid scores" $ do
        let kasu = withPoints Kasu deck
        let hikari = withPoints Hikari deck
        let pile = Game.makePile $ take 12 kasu ++ hikari
        Game.score pile `shouldBe` 13
