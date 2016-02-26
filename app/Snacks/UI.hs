module Snacks.UI
( View
, createView
, destroyView
) where

import qualified Snacks.Event as Event (Loop, post, Event(..))

import Control.Monad (liftM, forever)
import Control.Concurrent (ThreadId, forkIO)
import Data.Map.Strict as M (Map)

import qualified UI.HSCurses.Curses as Curses (
  Window
  , Key( KeyUp
        , KeyDown
        , KeyLeft
        , KeyRight
        , KeyChar
        )
  , CursorVisibility(CursorInvisible)
  , initScr
  , endWin
  , getCh
  , cBreak
  , echo
  , cursSet
  , keypad
  )

data View = View { loop :: Event.Loop, view :: Curses.Window }

createView :: Event.Loop -> IO View
createView loop = do
  view <- liftM (View loop) initCurses
  spawnInputThread view
  return view

destroyView :: View -> IO ()
destroyView _ = Curses.endWin

-- private
initCurses :: IO Curses.Window
initCurses = do
  window <- Curses.initScr
  Curses.cBreak True
  Curses.echo False
  Curses.cursSet Curses.CursorInvisible
  Curses.keypad window True
  return window

spawnInputThread :: View -> IO (ThreadId)
spawnInputThread view = forkIO $ forever $ Curses.getCh >>= post
  where post :: Curses.Key -> IO ()
        post = Event.post (loop view) . eventFor

        eventFor :: Curses.Key -> Event.Event
        eventFor key = case key of
                         Curses.KeyUp     -> Event.Up
                         Curses.KeyDown   -> Event.Down
                         Curses.KeyLeft   -> Event.Left
                         Curses.KeyRight  -> Event.Right
                         Curses.KeyChar c -> Event.Key c
                         otherwise        -> Event.None
