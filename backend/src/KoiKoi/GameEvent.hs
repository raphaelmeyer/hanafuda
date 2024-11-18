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
  = CardsDrawn Int
  | TableDealt Pile
  | HandDealt Player Hand
  deriving (Show)

data GameCommand
  = DealCards
  deriving (Show)

data Game a
  = EmitEvent GameEvent (() -> Game a)
  | DrawAndDealCards ((Int, Pile, Hand, Hand) -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Return a

emitEventM :: GameEvent -> Game ()
emitEventM event = EmitEvent event Return

drawAndDealCardsM :: Game (Int, Pile, Hand, Hand)
drawAndDealCardsM = DrawAndDealCards Return

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
  (>>=) (DrawAndDealCards callback) next =
    DrawAndDealCards (\cards -> (callback cards) >>= next)
  (>>=) (GetCommand cont) next =
    GetCommand (\command -> cont command >>= next)
  (>>=) (Return result) next = next result

processCommandM :: GameCommand -> Game (Maybe Result)
processCommandM DealCards = do
  -- isBeginning <- isBeginningOfRoundM
  -- if isBeginning then ...
  (drawn, table, oya, ko) <- drawAndDealCardsM
  emitEventM (CardsDrawn drawn)
  emitEventM (TableDealt table)
  emitEventM (HandDealt Oya oya)
  emitEventM (HandDealt Ko ko)
  pure Nothing

dealCards :: GameState -> (Int, Pile, Hand, Hand)
dealCards state =
  let cards = pileCards . gameStateDrawPile $ state
      table = makePile . take 8 $ cards
      oya = makeHand . take 8 . drop 8 $ cards
      ko = makeHand . take 8 . drop 16 $ cards
   in (24, table, oya, ko)

gameProcessEvent :: GameEvent -> GameState -> GameState
gameProcessEvent (CardsDrawn drawn) state =
  state
    { gameStateDrawPile = drawCards drawn . gameStateDrawPile $ state
    }
gameProcessEvent (TableDealt pile) state =
  state
    { gameStateTable = pile
    }
gameProcessEvent (HandDealt player hand) state =
  state
    { gameStateHands = dealHand player hand (gameStateHands state)
    }

runGame :: Game a -> GameState -> (Either (GameCommand -> Game a) a, GameState)
runGame (EmitEvent event cont) state =
  runGame (cont ()) (gameProcessEvent event state)
runGame (DrawAndDealCards cont) state =
  runGame (cont (dealCards state)) state
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
