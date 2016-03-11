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
  , drawFood
  , refresh
  )
import Snacks.World (
    World(snake, screen, food, speed)
  , Screen(Start, PrePlay, Playing)
  , Snake(dir)
  , createWorld
  )

import Control.Exception (finally)
import Control.Monad (forever)
import Control.Concurrent (ThreadId, forkIO, threadDelay, yield)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import System.Random (getStdGen)

data Engine = Engine { loop     :: Event.Loop
                     , view     :: UI.View
                     , world    :: World
                     , tickerId :: MVar ThreadId
                     }

createEngine :: Event.Loop -> UI.View -> IO (Engine)
createEngine l v = do
  gen  <- getStdGen
  mvar <- newEmptyMVar
  let w = createWorld gen $ UI.gridSize v
  return $ Engine { loop     = l
                  , view     = v
                  , world    = w
                  , tickerId = mvar
                  }

runEngine :: Engine -> IO ()
runEngine engine = do
  let engine' = updateWorld (\w -> w { screen = Start }) engine
  UI.drawScreen Start (view engine')
  UI.refresh (view engine')
  waitFor $ runLoop engine'

runLoop :: Engine -> IO ()
runLoop engine = do
  event <- Event.next (loop engine)
  case event of
    Event.Stop -> return ()
    otherwise  -> handleEvent event engine >>= runLoop

handleEvent :: Event.Event -> Engine -> IO Engine
handleEvent event engine = case (screen . world $ engine) of
  Start   -> startGame event engine
  PrePlay -> playGame  event engine
  Playing -> stepGame  event engine

startGame :: Event.Event -> Engine -> IO Engine
startGame _ engine = do
  let engine' = updateWorld (\w -> w { screen = PrePlay })  engine
  UI.drawSnake (snake . world $ engine') (view engine')
  UI.drawFood  (food  . world $ engine') (view engine')
  UI.drawScreen PrePlay (view engine')
  UI.refresh (view engine')
  return engine'

playGame :: Event.Event -> Engine -> IO Engine
playGame (Event.Dir d) engine = do
  let engine' = updateSnake (\s -> s { dir = d})
              . updateWorld (\w -> w { screen = Playing })
              $ engine
  startTicker engine'
  return engine'
playGame _ engine = return engine

stepGame :: Event.Event -> Engine -> IO Engine
stepGame Event.Tick = return
stepGame _ = return

updateWorld :: (World -> World) -> Engine -> Engine
updateWorld f engine = engine { world = f (world engine) }

updateSnake :: (Snake -> Snake) -> Engine -> Engine
updateSnake f engine = updateWorld g engine
  where g w = w { snake = f (snake w) }

startTicker :: Engine -> IO ()
startTicker engine = do
  threadId <- forkIO $ forever $ do
    Event.post (loop engine) Event.Tick
    yield
    threadDelay . round . (* 1000) . speed . world $ engine
  putMVar (tickerId engine) threadId

waitFor :: IO () -> IO ()
waitFor action = do
  flag <- newEmptyMVar :: IO (MVar ())
  forkIO $ action `finally` putMVar flag ()
  takeMVar flag >> return ()
