module KoiKoi.Game
  ( Hand,
    Pile,
    Score,
    Result (..),
    Player (..),
    emptyHand,
    makeHand,
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

makeHand :: [Card] -> Hand
makeHand = Hand . Set.fromList

newtype Pile = Pile {unPile :: Set.Set Cards.Card} deriving (Eq, Show)

emptyPile :: Pile
emptyPile = Pile Set.empty

makePile :: [Card] -> Pile
makePile = Pile . Set.fromList

pileCards :: Pile -> [Card]
pileCards = Set.toList . unPile

type Score = Int

data Result = Result
  { scoreOya :: Score,
    scoreKo :: Score
  }
  deriving (Eq, Show)

data Player = Oya | Ko deriving (Eq, Ord, Show)
