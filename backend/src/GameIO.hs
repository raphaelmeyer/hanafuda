module GameIO where

import KoiKoi.GameEvent
import KoiKoi.GameState
import qualified System.Random.Stateful as Random

gameIO :: IO (GameCommand -> IO ())
gameIO = do
  -- ref <- IORef.newIORef (gameLoopM, emptyGameState)
  state <- shuffleDeck emptyGameState Random.globalStdGen
  let processCommand command = do
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
