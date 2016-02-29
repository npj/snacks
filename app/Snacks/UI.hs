module Snacks.UI
( View
, createView
, destroyView
, size
, drawScreen
, drawSnake
, drawFood
, refresh
) where

import qualified Snacks.Event as Event (
    Loop
  , post
  , Event( Stop
         , Up
         , Down
         , Left
         , Right
         , Key
         , Arbitrary
         )
  )
import Snacks.World (
    Screen(Start)
  , Snake
  , Food
  )

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
  , Border
  , CursorVisibility(CursorInvisible)
  , wBorder
  , wRefresh
  , defaultBorder
  , mvWAddStr
  , initScr
  , endWin
  , getCh
  , cBreak
  , echo
  , cursSet
  , keypad
  , scrSize
  )

data View = View { loop :: Event.Loop, view :: Curses.Window }

createView :: Event.Loop -> IO View
createView loop = do
  view <- liftM (View loop) initCurses
  spawnInputThread view
  return view

destroyView :: View -> IO ()
destroyView _ = Curses.endWin

size :: View -> IO (Int, Int)
size = const Curses.scrSize

drawScreen :: Screen -> View -> IO ()
drawScreen Start = drawStartScreen

drawSnake :: Snake -> View -> IO ()
drawSnake = undefined

drawFood :: Food -> View -> IO ()
drawFood = undefined

drawStartScreen :: View -> IO ()
drawStartScreen view = do
  drawBorder view Curses.defaultBorder
  drawString view "Press any key to start"

refresh :: View -> IO ()
refresh = Curses.wRefresh . view

drawBorder :: View -> Curses.Border -> IO ()
drawBorder v border = Curses.wBorder (view v) border

drawString :: View -> String -> IO ()
drawString v string = do
  s <- size v
  let row = (fst s `div` 2)
      col = (snd s `div` 2) - (length string `div` 2)
  Curses.mvWAddStr (view v) row col string

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
                         otherwise        -> Event.Arbitrary
