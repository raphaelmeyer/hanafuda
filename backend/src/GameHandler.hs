module GameHandler (run) where

import GameIO
import KoiKoi.Game
import KoiKoi.GameEvent

run :: IO ()
run = do
  game <- gameIO
  game (DealHands emptyHand emptyHand)
