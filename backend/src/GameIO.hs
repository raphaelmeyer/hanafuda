module GameIO where

import KoiKoi.GameEvent

gameIO :: IO (GameCommand -> IO ())
gameIO = do
  -- ref <- IORef.newIORef (gameLoopM, emptyGameState)
  let state = emptyGameState
      processCommand command = do
        -- (next, state) <- IORef.readIORef ref
        print command
        -- let (step, state') = ...
        let (step, _) = runGame (gameLoopM command) state
        case step of
          -- Left next'
          Left _ -> do
            -- IORef.writeIORef ref (next', state')
            putStrLn "wait for next command"
          Right result -> do
            print result
            pure ()
        pure ()
  pure processCommand
