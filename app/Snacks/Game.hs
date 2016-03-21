module Snacks.Game
( Engine
, createEngine
, runEngine
) where

import Snacks.Types (Direction)

import qualified Snacks.Event as Event (
    Event(Stop, Dir, Tick)
  , Loop
  , next
  , post
  )
import qualified Snacks.UI as UI (
    View
  , gridSize
  , drawScreen
  , drawSnake
  , clearSnake
  , drawFood
  , refresh
  )
import Snacks.World (
    World(snake, screen, food, speed)
  , Screen(Start, PrePlay, Playing)
  , Snake
  , WorldUpdate(..)
  , createWorld
  , handleEvent
  )

import Snacks.Logging (Logger, createLogger, closeLogger, logStr)

import Control.Exception (finally)
import Control.Monad (liftM, forever)
import Control.Concurrent (ThreadId, forkIO, yield, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (mkStdGen)

data Engine = Engine { loop     :: Event.Loop
                     , view     :: UI.View
                     , world    :: World
                     , ticker   :: MVar ThreadId
                     , logger   :: Logger
                     }

createEngine :: Event.Loop -> UI.View -> IO (Engine)
createEngine l v = do
  time     <- liftM round getPOSIXTime
  t        <- newEmptyMVar
  debugLog <- createLogger "debug.txt"
  logStr "Created engine." debugLog
  let w = createWorld (mkStdGen time) $ UI.gridSize v
  return $ Engine { loop     = l
                  , view     = v
                  , world    = w
                  , ticker   = t
                  , logger   = debugLog
                  }

runEngine :: Engine -> IO ()
runEngine engine = do
  runUpdates [SetScreen Start] engine
  waitFor $ runLoop engine
  logStr "Shutting down" (logger engine)
  closeLogger . logger $ engine

runLoop :: Engine -> IO ()
runLoop engine = do
  event <- Event.next (loop engine)
  case event of
    Event.Stop -> return ()
    otherwise -> do
      let (world', updates) = handleEvent event (world engine)
      runUpdates updates engine
      runLoop (engine { world = world' })

runUpdates :: [WorldUpdate] -> Engine -> IO ()
runUpdates updates engine = do
  mapM_ (flip runUpdate $ engine) updates
  UI.refresh (view engine)

runUpdate :: WorldUpdate -> Engine -> IO ()
runUpdate (SetScreen PrePlay) engine = do
  UI.drawScreen PrePlay (view engine)
  UI.drawSnake (snake . world $ engine) (view engine)
  UI.drawFood  (food  . world $ engine) (view engine)

runUpdate (SetScreen Playing) engine = do
  startTicker engine
  UI.drawScreen Playing (view engine)

runUpdate (SetScreen s) engine = UI.drawScreen s (view engine)

runUpdate (MoveSnake old new) engine = do
  UI.clearSnake old (view engine)
  UI.drawSnake  new (view engine)

runUpdate (GrowSnake old new) engine = do
  UI.clearSnake old (view engine)
  UI.drawSnake  new (view engine)

runUpdate (NewFood old new) engine = do
  UI.drawFood new (view engine)

runUpdate (TurnSnake _ _) _ = return ()

startTicker :: Engine -> IO ()
startTicker engine = do
  tid <- forkIO $ forever $ do
    Event.post (loop engine) Event.Tick
    yield
    threadDelay . round . (* 1000) . speed . world $ engine
  putMVar (ticker engine) tid

waitFor :: IO () -> IO ()
waitFor action = do
  flag <- newEmptyMVar :: IO (MVar ())
  forkIO $ action `finally` putMVar flag ()
  takeMVar flag >> return ()
