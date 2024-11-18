module KoiKoi.Game
  ( Hand,
    Pile,
    emptyHand,
    emptyPile,
    makePile,
    pileCards,
  )
where

import Cards
import qualified Data.Set as Set

newtype Hand = Hand {unHand :: Set.Set Cards.Card} deriving (Eq, Show)

emptyHand :: Hand
emptyHand = Hand Set.empty

newtype Pile = Pile {unPile :: Set.Set Cards.Card} deriving (Eq, Show)

emptyPile :: Pile
emptyPile = Pile Set.empty

makePile :: [Card] -> Pile
makePile = Pile . Set.fromList

pileCards :: Pile -> [Card]
pileCards = Set.toList . unPile
