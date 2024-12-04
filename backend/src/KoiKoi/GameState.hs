module KoiKoi.GameState
  ( GameState,
    emptyGameState,
    shuffleDeck,
    dealToTable,
    dealHand,
  )
where

import qualified Cards
import KoiKoi.Game
import qualified System.Random.Stateful as Random

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

shuffleDeck :: (Random.StatefulGen g m) => GameState -> g -> m GameState
shuffleDeck state g = do
  shuffled <- shuffle Cards.deck g
  pure
    state
      { gameStateDrawPile = makePile shuffled,
        gameStateTable = emptyPile,
        gameStateHands =
          PlayerHands
            { playerHandOya = emptyHand,
              playerHandKo = emptyHand
            }
      }

dealToTable :: GameState -> GameState
dealToTable state =
  state
    { gameStateDrawPile = drawPile,
      gameStateTable = addToPile cards (gameStateTable state)
    }
  where
    (cards, drawPile) = drawFromPile 8 (gameStateDrawPile state)

dealHand :: Player -> GameState -> GameState
dealHand Oya state =
  state
    { gameStateDrawPile = drawPile,
      gameStateHands = hands {playerHandOya = addToHand cards . playerHandOya . gameStateHands $ state}
    }
  where
    hands = gameStateHands state
    (cards, drawPile) = drawFromPile 8 (gameStateDrawPile state)
dealHand Ko state =
  state
    { gameStateDrawPile = drawPile,
      gameStateHands = hands {playerHandKo = addToHand cards . playerHandKo . gameStateHands $ state}
    }
  where
    hands = gameStateHands state
    (cards, drawPile) = drawFromPile 8 (gameStateDrawPile state)

shuffle :: (Random.StatefulGen g m) => [a] -> g -> m [a]
shuffle [] _ = pure []
shuffle list g = do
  i <- Random.uniformRM (0, length list - 1) g
  let (front, back) = splitAt i list
  let x = head back
  xs <- shuffle (tail back ++ front) g
  pure $ x : xs
