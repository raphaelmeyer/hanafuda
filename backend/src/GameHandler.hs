module GameHandler (run) where

import GameIO
import KoiKoi.GameEvent

run :: IO ()
run = do
  game <- gameIO
  game DealCards
