module GameIO where

import qualified Cards
import qualified Cards as Card
import KoiKoi.Game
import KoiKoi.GameEvent
import KoiKoi.GameState

shuffle :: [Card.Card] -> IO [Card.Card]
shuffle = pure

gameIO :: IO (GameCommand -> IO ())
gameIO = do
  -- ref <- IORef.newIORef (gameLoopM, emptyGameState)
  deck <- shuffle Cards.deck
  let state =
        emptyGameState
          { gameStateDrawPile = makePile deck
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
