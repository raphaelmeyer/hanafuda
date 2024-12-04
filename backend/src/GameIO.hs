module GameIO where

import qualified Cards
import KoiKoi.Game
import KoiKoi.GameEvent
import KoiKoi.GameState
import qualified System.Random.Stateful as Random

gameIO :: IO (GameCommand -> IO ())
gameIO = do
  -- ref <- IORef.newIORef (gameLoopM, emptyGameState)
  deck <- shufflePile (makePile Cards.deck) Random.globalStdGen
  let state =
        emptyGameState
          { gameStateDrawPile = deck
          }
      processCommand command = do
        -- (next, state) <- IORef.readIORef ref
        print command
        let (step, state') = runGame (gameLoopM command) state
        case step of
          -- Left next'
          Left _ -> do
            -- IORef.writeIORef ref (next', state')
            putStrLn "Wait for next command"
            print state'
          Right result -> do
            print result
            pure ()
        pure ()
  pure processCommand
