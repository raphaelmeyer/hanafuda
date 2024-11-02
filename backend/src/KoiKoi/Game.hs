module KoiKoi.Game (Pile, emptyPile, makePile, score) where

import Cards
import qualified Data.Set as Set

newtype Hand = Hand {unHand :: Set.Set Cards.Card} deriving (Eq, Show)

newtype Pile = Pile {unPile :: Set.Set Cards.Card} deriving (Eq, Show)

emptyPile :: Pile
emptyPile = Pile Set.empty

makePile :: [Card] -> Pile
makePile = Pile . Set.fromList

score :: Pile -> Int
score pile = scoreHikari cards + scoreKasu cards + scoreSpecials cards + scoreTane cards
  where
    cards = Set.toList . unPile $ pile

scoreHikari :: [Card] -> Int
scoreHikari cards = case (length hikari, raining) of
  (5, _) -> 10
  (4, False) -> 8
  (4, True) -> 7
  (3, False) -> 5
  _ -> 0
  where
    hikari = withRank Hikari cards
    raining = any (hasName Michikaze) hikari

scoreTane :: [Card] -> Int
scoreTane cards
  | inoshikachou = 5 + tane - 3
  | tane >= 5 = tane - 4
  | otherwise = 0
  where
    tane = length . withRank Tane $ cards
    inoshikachou =
      elemName Yamajishi cards
        && elemName Shika cards
        && elemName Chou cards

scoreKasu :: [Card] -> Int
scoreKasu cards = if kasu >= 10 then kasu - 9 else 0
  where
    kasu = length . withRank Kasu $ cards

scoreSpecials :: [Card] -> Int
scoreSpecials cards =
  (if sakura && sake then 5 else 0)
    + (if tsuki && sake then 5 else 0)
  where
    sakura = elemName Mankai cards
    tsuki = elemName Mochizuki cards
    sake = elemName Sakazuki cards
