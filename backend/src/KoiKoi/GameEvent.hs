{-# LANGUAGE InstanceSigs #-}

module KoiKoi.GameEvent (gameLoopM, runGame, emptyGameState) where

import KoiKoi.Game

data GameEvent
  = HandDealt Player Hand

data GameCommand
  = DealHands Hand Hand -- oya ko

data Game a
  = EmitEvent GameEvent (() -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Return a

emitEventM :: GameEvent -> Game ()
emitEventM event = EmitEvent event Return

instance Functor Game where
  fmap :: (a -> b) -> Game a -> Game b
  fmap = undefined

instance Applicative Game where
  pure :: a -> Game a
  pure = Return

  (<*>) :: Game (a -> b) -> Game a -> Game b
  (<*>) = undefined

instance Monad Game where
  (>>=) :: Game a -> (a -> Game b) -> Game b
  (>>=) (EmitEvent event callback) next =
    EmitEvent event (const (callback () >>= next))
  (>>=) (GetCommand cont) next =
    GetCommand (\command -> cont command >>= next)
  (>>=) (Return result) next = next result

processCommandM :: GameCommand -> Game (Maybe Result)
processCommandM (DealHands oya ko) = do
  emitEventM (HandDealt Oya oya)
  emitEventM (HandDealt Ko ko)
  pure Nothing

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

gameProcessEvent :: GameEvent -> GameState -> GameState
gameProcessEvent (HandDealt player hand) state =
  state
    { gameStateHands = dealHand player hand (gameStateHands state)
    }

runGame :: Game a -> GameState -> (Either (GameCommand -> Game a) a, GameState, [GameEvent])
runGame (EmitEvent event cont) state =
  runGame (cont ()) (gameProcessEvent event state)
runGame _ _ = undefined

gameLoopM :: GameCommand -> Game Result
gameLoopM command = do
  maybeResult <- processCommandM command
  case maybeResult of
    Nothing -> GetCommand gameLoopM
    Just result -> pure result
