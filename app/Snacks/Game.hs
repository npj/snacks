module Snacks.Game
( Engine
, createEngine
, runEngine
) where

import qualified Snacks.Event as Event (Event(Stop), Loop, next)
import qualified Snacks.UI as UI (
    View
  , size
  , drawScreen
  , drawSnake
  , drawFood
  , refresh
  )
import Snacks.World (
    World(wSnake, wScreen, wFood)
  , Screen(Start, PreStart, NoScreen)
  , createWorld
  , createSnake
  , createFood
  )

import Control.Exception (finally)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import System.Random (getStdGen)

import Debug.Trace

data Engine = Engine { loop  :: Event.Loop
                     , view  :: UI.View
                     , world :: World
                     }

createEngine :: Event.Loop -> UI.View -> IO (Engine)
createEngine loop view = do
  viewSize <- UI.size view
  return $ Engine loop view $ createWorld viewSize

runEngine :: Engine -> IO ()
runEngine engine = do
  let engine' = updateWorld engine $ \w ->
                  w { wScreen = Start }
  waitFor . runLoop $ engine'

runLoop :: Engine -> IO ()
runLoop engine = do
  event <- Event.next (loop engine)
  case event of
    Event.Stop -> return ()
    otherwise  -> handleEvent event engine >>= runLoop

handleEvent :: Event.Event -> Engine -> IO Engine
handleEvent event engine =
  case (wScreen . world $ engine) of
    Start     -> startGame engine
    otherwise -> return engine

startGame :: Engine -> IO Engine
startGame engine = do
  gen     <- getStdGen
  let engine' = updateWorld engine $ \w ->
                  w { wSnake  = createSnake gen
                    , wFood   = createFood gen
                    , wScreen = PreStart
                    }
  UI.drawSnake (wSnake . world $ engine') (view engine')
  UI.drawFood  (wFood  . world $ engine') (view engine')
  UI.drawScreen PreStart (view engine')
  UI.refresh (view engine')
  return engine'

updateWorld :: Engine -> (World -> World) -> Engine
updateWorld engine f = engine { world = f (world engine) }

waitFor :: IO () -> IO ()
waitFor action = do
  flag <- newEmptyMVar :: IO (MVar ())
  forkIO $ action `finally` putMVar flag ()
  takeMVar flag >> return ()
