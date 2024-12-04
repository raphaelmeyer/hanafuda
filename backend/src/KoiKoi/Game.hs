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
    shufflePile,
  )
where

import Cards
import qualified Data.Set as Set
import qualified System.Random.Stateful as Random

newtype Hand = Hand {unHand :: Set.Set Cards.Card} deriving (Eq, Show)

emptyHand :: Hand
emptyHand = Hand Set.empty

makeHand :: [Card] -> Hand
makeHand = Hand . Set.fromList

newtype Pile = Pile {unPile :: [Cards.Card]} deriving (Eq, Show)

emptyPile :: Pile
emptyPile = Pile []

makePile :: [Card] -> Pile
makePile = Pile

pileCards :: Pile -> [Card]
pileCards = unPile

shufflePile :: (Random.StatefulGen g m) => Pile -> g -> m Pile
shufflePile (Pile cards) g = do
  shuffled <- shuffle cards g
  pure (Pile shuffled)

shuffle :: (Random.StatefulGen g m) => [a] -> g -> m [a]
shuffle [] _ = pure []
shuffle list g = do
  i <- Random.uniformRM (0, length list - 1) g
  let (front, back) = splitAt i list
  let x = head back
  xs <- shuffle (tail back ++ front) g
  pure $ x : xs

type Score = Int

data Result = Result
  { scoreOya :: Score,
    scoreKo :: Score
  }
  deriving (Eq, Show)

data Player = Oya | Ko deriving (Eq, Ord, Show)
