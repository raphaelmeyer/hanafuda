module KoiKoi.GameState where

import KoiKoi.Game

data PlayerHands = PlayerHands
  { playerHandOya :: Hand,
    playerHandKo :: Hand
  }
  deriving (Show)

data GameState = GameState
  { gameStateDrawPile :: Pile,
    gameStateTable :: Pile,
    gameStateHands :: PlayerHands
  }
  deriving (Show)

emptyGameState :: GameState
emptyGameState =
  GameState
    { gameStateDrawPile = emptyPile,
      gameStateTable = emptyPile,
      gameStateHands =
        PlayerHands
          { playerHandOya = emptyHand,
            playerHandKo = emptyHand
          }
    }

dealHand :: Player -> Hand -> PlayerHands -> PlayerHands
dealHand Oya hand hands =
  hands
    { playerHandOya = hand
    }
dealHand Ko hand hands =
  hands
    { playerHandKo = hand
    }

drawCards :: Int -> Pile -> Pile
drawCards n = makePile . drop n . pileCards
