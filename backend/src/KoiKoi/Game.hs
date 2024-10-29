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
score pile = scoreHikari cards + scoreKasu cards + scoreSpecials cards
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

scoreKasu :: [Card] -> Int
scoreKasu cards = if kasu >= 10 then kasu - 9 else 0
  where
    kasu = length . withRank Kasu $ cards

scoreSpecials :: [Card] -> Int
scoreSpecials cards = (* 5) . length . filter id $ sets
  where
    sakura = any (hasName Mankai) cards
    tsuki = any (hasName Mochizuki) cards
    sake = any (hasName Sakazuki) cards
    ino = any (hasName Yamajishi) cards
    shika = any (hasName Shika) cards
    chou = any (hasName Chou) cards
    sets = [sakura && sake, tsuki && sake, ino && shika && chou]
