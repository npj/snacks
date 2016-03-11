module Snacks.UI
( View
, createView
, destroyView
, gridSize
, drawScreen
, drawSnake
, drawFood
, refresh
) where

import Snacks.Types (Direction(DirUp, DirDown, DirLeft, DirRight))

import qualified Snacks.Event as Event (
    Loop
  , post
  , Event( Stop
         , Dir
         , Key
         , Noop
         )
  )
import Snacks.World (
    Screen(Start, PrePlay)
  , Snake
  , Food
  )

import Control.Monad (forever)
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
  , initCurses
  , newWin
  , wBorder
  , wRefresh
  , stdScr
  , refresh
  , update
  , defaultBorder
  , mvWAddStr
  , endWin
  , getCh
  , cBreak
  , echo
  , cursSet
  , keypad
  , scrSize
  )

data View = View { loop   :: Event.Loop
                 , window :: Curses.Window
                 , size   :: (Int, Int)
                 }

createView :: Event.Loop -> IO View
createView loop = do
  setupCurses
  view <- adjustedView loop
  spawnInputThread view
  return view

setupCurses :: IO ()
setupCurses = do
  Curses.initCurses
  Curses.cBreak True
  Curses.echo False
  Curses.cursSet Curses.CursorInvisible
  return ()

adjustedView :: Event.Loop -> IO View
adjustedView loop = do
  (rows, cols) <- Curses.scrSize
  let cols' = (cols `div` 2) * 2
  window <- Curses.newWin rows cols' 0 0
  Curses.keypad window True
  return $ View loop window (rows, cols')

destroyView :: View -> IO ()
destroyView _ = Curses.endWin

gridSize :: View -> (Int, Int)
gridSize view = (rows - 2, cols `div` 2 - 2)
  where (rows, cols) = size view

drawScreen :: Screen -> View -> IO ()
drawScreen Start   = drawStartScreen
drawScreen PrePlay = drawPrePlayScreen

drawSnake :: Snake -> View -> IO ()
drawSnake _ _ = return ()

drawFood :: Food -> View -> IO ()
drawFood _ _ = return ()

drawStartScreen :: View -> IO ()
drawStartScreen view = do
  drawBorder view Curses.defaultBorder
  drawPrompt view "Press any key to start"

drawPrePlayScreen :: View -> IO ()
drawPrePlayScreen view = do
  drawBorder view Curses.defaultBorder
  clearPrompt view

refresh :: View -> IO ()
refresh = Curses.wRefresh . window

drawBorder :: View -> Curses.Border -> IO ()
drawBorder v border = Curses.wBorder (window v) border

drawPrompt :: View -> String -> IO ()
drawPrompt v string = do
  let (rows, cols) = size v
      row          = (rows `div` 2)
      col          = (cols `div` 2) - (length string `div` 2)
  Curses.mvWAddStr (window v) row col string

clearPrompt :: View -> IO ()
clearPrompt v = do
  let s   = size v
      row = (fst s `div` 2)
      str = replicate (snd s - 2) ' '
  Curses.mvWAddStr (window v) row 1 str

spawnInputThread :: View -> IO ThreadId
spawnInputThread view = do
  forkIO $ forever $ Curses.getCh >>= post
  where post :: Curses.Key -> IO ()
        post = Event.post (loop view) . eventFor

        eventFor :: Curses.Key -> Event.Event
        eventFor key = case key of
                         Curses.KeyUp     -> Event.Dir DirUp
                         Curses.KeyDown   -> Event.Dir DirDown
                         Curses.KeyLeft   -> Event.Dir DirLeft
                         Curses.KeyRight  -> Event.Dir DirRight
                         Curses.KeyChar c -> Event.Key c
                         otherwise        -> Event.Noop
