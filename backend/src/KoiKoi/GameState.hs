module KoiKoi.GameState where

import KoiKoi.Game

data PlayerHands = PlayerHands
  { playerHandOya :: Hand,
    playerHandKo :: Hand
  }

newtype GameState = GameState
  { gameStateHands :: PlayerHands
  }

emptyGameState :: GameState
emptyGameState =
  GameState
    { gameStateHands =
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
