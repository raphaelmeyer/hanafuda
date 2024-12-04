module KoiKoi.Game
  ( Hand,
    Pile,
    Score,
    Result (..),
    Player (..),
    emptyHand,
    makeHand,
    addToHand,
    emptyPile,
    makePile,
    addToPile,
    drawFromPile,
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

addToHand :: [Card] -> Hand -> Hand
addToHand cards (Hand hand) = Hand (foldr Set.insert hand cards)

newtype Pile = Pile {unPile :: [Cards.Card]} deriving (Eq, Show)

emptyPile :: Pile
emptyPile = Pile []

makePile :: [Card] -> Pile
makePile = Pile

addToPile :: [Card] -> Pile -> Pile
addToPile cards (Pile pile) = Pile (pile ++ cards)

drawFromPile :: Int -> Pile -> ([Cards.Card], Pile)
drawFromPile n (Pile pile) = (take n pile, Pile $ drop n pile)

pileCards :: Pile -> [Card]
pileCards = unPile

type Score = Int

data Result = Result
  { scoreOya :: Score,
    scoreKo :: Score
  }
  deriving (Eq, Show)

data Player = Oya | Ko deriving (Eq, Ord, Show)
