module Snacks.Game
( Engine
, createEngine
, runEngine
) where

import qualified Snacks.Event as Event (Event(Stop), Loop, next)
import qualified Snacks.UI as UI

import Control.Exception (finally)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

data Engine = Engine { loop :: Event.Loop, view :: UI.View }

createEngine :: Event.Loop -> UI.View -> IO (Engine)
createEngine loop view = return $ Engine loop view

runEngine :: Engine -> IO ()
runEngine = waitFor . runLoop . loop

runLoop :: Event.Loop -> IO ()
runLoop loop = do
  event <- Event.next loop
  case event of
    Event.Stop -> return ()
    otherwise  -> runLoop loop

waitFor :: IO () -> IO ()
waitFor action = do
  flag <- newEmptyMVar :: IO (MVar ())
  forkIO $ action `finally` putMVar flag ()
  takeMVar flag >> return ()

