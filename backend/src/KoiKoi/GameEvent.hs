{-# LANGUAGE InstanceSigs #-}

module KoiKoi.GameEvent
  ( GameEvent,
    GameCommand (..),
    gameLoopM,
    runGame,
  )
where

import KoiKoi.Game
import KoiKoi.GameState

data GameEvent
  = TableDealt
  | HandDealt Player
  deriving (Show)

data GameCommand
  = DealCards
  deriving (Show)

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
processCommandM DealCards = do
  -- isBeginning <- isBeginningOfRoundM
  -- if isBeginning then ...
  emitEventM TableDealt
  emitEventM (HandDealt Oya)
  emitEventM (HandDealt Ko)
  pure Nothing

gameProcessEvent :: GameEvent -> GameState -> GameState
gameProcessEvent TableDealt state = dealToTable state
gameProcessEvent (HandDealt player) state = dealHand player state

runGame :: Game a -> GameState -> (Either (GameCommand -> Game a) a, GameState)
runGame (EmitEvent event cont) state =
  runGame (cont ()) (gameProcessEvent event state)
runGame (GetCommand cont) state =
  (Left cont, state)
runGame (Return result) state =
  (Right result, state)

gameLoopM :: GameCommand -> Game Result
gameLoopM command = do
  maybeResult <- processCommandM command
  case maybeResult of
    Nothing -> GetCommand gameLoopM
    Just result -> pure result
