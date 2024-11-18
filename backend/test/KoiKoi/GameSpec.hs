module KoiKoi.GameSpec where

import Cards
import qualified KoiKoi.Game as Game
import KoiKoi.Score
import Test.Hspec

spec :: Spec
spec = do
  describe "scoring" $ do
    describe "empty pile" $ do
      it "scores no points" $ do
        score Game.emptyPile `shouldBe` 0

    describe "kasu" $ do
      let kasu = withRank Kasu deck

      it "scores no points for less than ten cards" $ do
        (score . Game.makePile . take 1 $ kasu) `shouldBe` 0
        (score . Game.makePile . take 7 $ kasu) `shouldBe` 0
        (score . Game.makePile . take 9 $ kasu) `shouldBe` 0

      it "scores one point for ten cards" $ do
        (score . Game.makePile . take 10 $ kasu) `shouldBe` 1

      it "scores one additional point for each additional card" $ do
        (score . Game.makePile . take 11 $ kasu) `shouldBe` 2
        (score . Game.makePile . take 14 $ kasu) `shouldBe` 5

    describe "tan" $ do
      let akatan = withRank Akatan deck
      let aotan = withRank Aotan deck
      let tanzaku = withRank Tanzaku deck

      describe "akatan" $ do
        it "scores five points for all three akatan cards" $ do
          let pile = Game.makePile akatan
          score pile `shouldBe` 5

        it "scores one additional point for each additional tan card" $ do
          let pile = Game.makePile (akatan ++ take 1 aotan ++ take 1 tanzaku)
          score pile `shouldBe` 7

      describe "aotan" $ do
        it "scores five points for all three aotan cards" $ do
          let pile = Game.makePile aotan
          score pile `shouldBe` 5

        it "scores one additional point for each additional tan card" $ do
          let pile = Game.makePile (aotan ++ take 2 akatan ++ take 1 tanzaku)
          score pile `shouldBe` 8

      describe "akatan and aotan duplication" $ do
        it "scores ten points for all six akatan and aotan cards" $ do
          let pile = Game.makePile (akatan ++ aotan)
          score pile `shouldBe` 10

        it "scores one additional point for each additional tan card" $ do
          let pile = Game.makePile (akatan ++ aotan ++ take 1 tanzaku)
          score pile `shouldBe` 11

      it "scores one point for five tan cards" $ do
        let pile = Game.makePile (take 2 akatan ++ take 1 aotan ++ take 2 tanzaku)
        score pile `shouldBe` 1

      it "scores one additional point for each additional tan card" $ do
        let pile = Game.makePile (take 1 akatan ++ take 2 aotan ++ take 4 tanzaku)
        score pile `shouldBe` 3

    describe "tane" $ do
      describe "ino-shika-chou" $ do
        it "scores five points for ino-shika-chou" $ do
          let ino =
                Game.makePile
                  [ fromName Yamajishi,
                    fromName Shika,
                    fromName Chou
                  ]
          score ino `shouldBe` 5

        it "scores one additional point for each additional tane card" $ do
          let pile =
                Game.makePile
                  [ fromName Yamajishi,
                    fromName Shika,
                    fromName Chou,
                    fromName Yatsuhashi,
                    fromName Kari
                  ]
          score pile `shouldBe` 7

      it "scores no points for less than five cards" $ do
        let pile =
              Game.makePile
                [ fromName Hototogisu,
                  fromName Chou,
                  fromName Kari,
                  fromName Tsubame
                ]
        score pile `shouldBe` 0

      it "scores one point for five cards" $ do
        let pile =
              Game.makePile
                [ fromName Hototogisu,
                  fromName Yatsuhashi,
                  fromName Chou,
                  fromName Kari,
                  fromName Tsubame
                ]
        score pile `shouldBe` 1

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
        score pile `shouldBe` 3

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
        score pile `shouldBe` 10

      it "scores eight points for four hikari cards excluding Ono no Michikaze" $ do
        let pile =
              Game.makePile
                [ fromName Tsuru,
                  fromName Mankai,
                  fromName Mochizuki,
                  fromName Houou
                ]
        score pile `shouldBe` 8

      it "scores seven points for four hikari cards including Ono no Michikaze" $ do
        let pile =
              Game.makePile
                [ fromName Tsuru,
                  fromName Mochizuki,
                  fromName Michikaze,
                  fromName Houou
                ]
        score pile `shouldBe` 7

      it "scores five points for three hikari cards excluding Ono no Michikaze" $ do
        let pile =
              Game.makePile
                [ fromName Tsuru,
                  fromName Mankai,
                  fromName Houou
                ]
        score pile `shouldBe` 5

      it "scores no points for other combinations" $ do
        let pile =
              Game.makePile
                [ fromName Mankai,
                  fromName Michikaze,
                  fromName Houou
                ]
        score pile `shouldBe` 0

    describe "special combinations" $ do
      it "scores five points for hanami ni ippai" $ do
        let hanami = Game.makePile [fromName Mankai, fromName Sakazuki]
        score hanami `shouldBe` 5

      it "scores five points for tsukimi ni ippai" $ do
        let tsukimi = Game.makePile [fromName Mochizuki, fromName Sakazuki]
        score tsukimi `shouldBe` 5

    describe "whole pile" $ do
      let hikari = withRank Hikari deck
      let akatan = withRank Akatan deck
      let aotan = withRank Aotan deck
      let tanzaku = withRank Tanzaku deck
      let kasu = withRank Kasu deck

      it "should score no points for a pile with no yaku" $ do
        let pile =
              Game.makePile
                ( take 9 kasu
                    ++ take 2 akatan
                    ++ take 2 aotan
                    ++ [fromName Yamajishi, fromName Chou, fromName Hototogisu, fromName Tsubame]
                    ++ [fromName Tsuru, fromName Mochizuki, fromName Michikaze]
                )
        score pile `shouldBe` 0

      it "should add up all valid scores" $ do
        let pile =
              Game.makePile
                ( take 12 kasu
                    ++ hikari
                    ++ aotan
                    ++ (take 2 tanzaku)
                    ++ [fromName Sakazuki]
                )
        score pile `shouldBe` 30
