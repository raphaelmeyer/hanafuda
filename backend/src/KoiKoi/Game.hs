module KoiKoi.Game (Pile, emptyPile, makePile, score) where

import Cards
import qualified Data.Set as Set

newtype Hand = Hand {unHand :: Set.Set Cards.Card} deriving (Eq, Show)

newtype Pile = Pile {unPile :: Set.Set Cards.Card} deriving (Eq, Show)

emptyPile :: Pile
emptyPile = Pile Set.empty

makePile :: [Cards.Card] -> Pile
makePile = Pile . Set.fromList

score :: Pile -> Int
score pile = if kasu >= 10 then kasu - 9 else 0
  where
    kasu = length . withPoints Cards.Kasu . Set.toList . unPile $ pile
