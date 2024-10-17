module KoiKoi.Game (Pile, emptyPile, makePile, withPoints, score) where

import Cards
import qualified Data.Set as Set

newtype Hand = Hand {unHand :: Set.Set Cards.Card} deriving (Eq, Show)

newtype Pile = Pile {unPile :: Set.Set Cards.Card} deriving (Eq, Show)

emptyPile :: Pile
emptyPile = Pile Set.empty

makePile :: [Cards.Card] -> Pile
makePile = Pile . Set.fromList

score :: Pile -> Int
score pile = if kasu == 10 then 1 else 0
  where
    kasu = length . withPoints Cards.Kasu $ pile

-- isSuit :: Suit -> Card -> Bool
-- isSuit s = (== s) . suit

hasPoints :: Points -> Card -> Bool
hasPoints p = (== p) . points

-- hasName :: Name -> Card -> Bool
-- hasName n = (== n) . name

withPoints :: Points -> Pile -> [Card]
withPoints p = Set.toList . Set.filter (hasPoints p) . unPile
