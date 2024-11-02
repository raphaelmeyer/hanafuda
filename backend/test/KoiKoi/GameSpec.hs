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
      let kasu = withRank Kasu deck

      it "scores no points for less than ten cards" $ do
        (Game.score . Game.makePile . take 1 $ kasu) `shouldBe` 0
        (Game.score . Game.makePile . take 7 $ kasu) `shouldBe` 0
        (Game.score . Game.makePile . take 9 $ kasu) `shouldBe` 0

      it "scores one point for ten cards" $ do
        (Game.score . Game.makePile . take 10 $ kasu) `shouldBe` 1

      it "scores one additional point for each additional card" $ do
        (Game.score . Game.makePile . take 11 $ kasu) `shouldBe` 2
        (Game.score . Game.makePile . take 14 $ kasu) `shouldBe` 5

    describe "tane" $ do
      it "scores five points for ino-shika-chou" $ do
        let ino =
              Game.makePile
                [ fromName Yamajishi,
                  fromName Shika,
                  fromName Chou
                ]
        Game.score ino `shouldBe` 5

      it "scores one additional point for each additional tane card" $ do
        let pile =
              Game.makePile
                [ fromName Yamajishi,
                  fromName Shika,
                  fromName Chou,
                  fromName Yatsuhashi,
                  fromName Kari
                ]
        Game.score pile `shouldBe` 7

      it "scores no points for less than five cards" $ do
        let pile =
              Game.makePile
                [ fromName Hototogisu,
                  fromName Chou,
                  fromName Kari,
                  fromName Tsubame
                ]
        Game.score pile `shouldBe` 0

      it "scores one point for five cards" $ do
        let pile =
              Game.makePile
                [ fromName Hototogisu,
                  fromName Yatsuhashi,
                  fromName Chou,
                  fromName Kari,
                  fromName Tsubame
                ]
        Game.score pile `shouldBe` 1

      it "scores one additional point for each additional tane card" $ do
        let pile =
              Game.makePile
                [ fromName Uguisu,
                  fromName Hototogisu,
                  fromName Yatsuhashi,
                  fromName Yamajishi,
                  fromName Kari,
                  fromName Sakazuki,
                  fromName Tsubame
                ]
        Game.score pile `shouldBe` 3

    describe "hikari" $ do
      it "scores ten points for all five hikari cards" $ do
        let pile =
              Game.makePile
                [ fromName Tsuru,
                  fromName Mankai,
                  fromName Mochizuki,
                  fromName Michikaze,
                  fromName Houou
                ]
        Game.score pile `shouldBe` 10

      it "scores eight points for four hikari cards excluding Ono no Michikaze" $ do
        let pile =
              Game.makePile
                [ fromName Tsuru,
                  fromName Mankai,
                  fromName Mochizuki,
                  fromName Houou
                ]
        Game.score pile `shouldBe` 8

      it "scores seven points for four hikari cards including Ono no Michikaze" $ do
        let pile =
              Game.makePile
                [ fromName Tsuru,
                  fromName Mochizuki,
                  fromName Michikaze,
                  fromName Houou
                ]
        Game.score pile `shouldBe` 7

      it "scores five points for three hikari cards excluding Ono no Michikaze" $ do
        let pile =
              Game.makePile
                [ fromName Tsuru,
                  fromName Mankai,
                  fromName Houou
                ]
        Game.score pile `shouldBe` 5

      it "scores no points for other combinations" $ do
        let pile =
              Game.makePile
                [ fromName Mankai,
                  fromName Michikaze,
                  fromName Houou
                ]
        Game.score pile `shouldBe` 0

    describe "special combinations" $ do
      it "scores five points for hanami ni ippai" $ do
        let hanami = Game.makePile [fromName Mankai, fromName Sakazuki]
        Game.score hanami `shouldBe` 5

      it "scores five points for tsukimi ni ippai" $ do
        let tsukimi = Game.makePile [fromName Mochizuki, fromName Sakazuki]
        Game.score tsukimi `shouldBe` 5

    describe "whole pile" $ do
      it "should add up all valid scores" $ do
        let kasu = withRank Kasu deck
        let hikari = withRank Hikari deck
        let pile = Game.makePile $ take 12 kasu ++ hikari
        Game.score pile `shouldBe` 13
