module Snacks.UI
( View
, createView
, destroyView
, gridSize
, drawScreen
, drawFood
, drawSnake
, clearSnake
, refresh
) where

import Snacks.Types (Direction(DirUp, DirDown, DirLeft, DirRight))

import qualified Snacks.Event as Event (
    Loop
  , post
  , Event( Stop
         , Dir
         , Key
         , Tick
         , Noop
         )
  )
import Snacks.World (
    Screen(Start, PrePlay, GameOver)
  , Snake(dir, body)
  , Food(position)
  , WorldUpdate(..)
  )

import Control.Monad (forever, forM_)
import Control.Concurrent (forkIO)
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
  , initScr
  , newWin
  , wRefresh
  , stdScr
  , refresh
  , update
  , mvWAddStr
  , endWin
  , getCh
  , cBreak
  , echo
  , cursSet
  , keypad
  , scrSize
  , diamond
  , lantern
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
  window <- Curses.initScr
  Curses.cBreak True
  Curses.echo False
  Curses.cursSet Curses.CursorInvisible
  Curses.keypad window True
  return ()

adjustedView :: Event.Loop -> IO View
adjustedView loop = do
  (rows, cols) <- Curses.scrSize
  let cols' = (cols `div` 2) * 2
  window <- Curses.newWin rows cols' 0 0
  return $ View loop window (rows, cols')

destroyView :: View -> IO ()
destroyView _ = Curses.endWin

gridSize :: View -> (Int, Int)
gridSize view = (rows - 1, cols `div` 2 - 2)
  where (rows, cols) = size view

drawSnake :: Snake -> View -> IO ()
drawSnake snake view =
  forM_ (body snake) $ \(row, col) -> do
    Curses.mvWAddStr (window view) row (col * 2) [Curses.diamond]

clearSnake :: Snake -> View -> IO ()
clearSnake snake view =
  forM_ (body snake) $ \(row, col) -> do
    Curses.mvWAddStr (window view) row (col * 2) " "

drawFood :: Food -> View -> IO ()
drawFood food view =
  Curses.mvWAddStr (window view) row (col * 2) "☰"
  where (row, col) = (position food)

drawScreen :: Screen -> View -> IO ()
drawScreen Start   = drawStartScreen
drawScreen PrePlay = drawPrePlayScreen
drawScreen GameOver = drawGameOverScreen
drawScreen _ = return . const ()

drawStartScreen :: View -> IO ()
drawStartScreen view = do
  drawPrompt view "Press any key to start"
  drawBorder view

drawPrePlayScreen :: View -> IO ()
drawPrePlayScreen view = do
  clearPrompt view
  drawBorder view

drawGameOverScreen :: View -> IO ()
drawGameOverScreen view = do
  drawPrompt view "u ded"

refresh :: View -> IO ()
refresh = Curses.wRefresh . window

drawBorder :: View -> IO ()
drawBorder v = do
  let (top, left)       = (0, 0)
      (bottom', right') = size v
      (bottom, right)   = (bottom' - 1, right' - 2)
  forM_ [(top + 1)..(bottom - 1)] $ \row -> do
    Curses.mvWAddStr (window v) row left  "|"
    Curses.mvWAddStr (window v) row right "|"
  forM_ [left..right - 1] $ \col -> do
    Curses.mvWAddStr (window v) top    col  "-"
    Curses.mvWAddStr (window v) bottom col  "-"

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

spawnInputThread :: View -> IO ()
spawnInputThread view = do
  forkIO $ forever $ Curses.getCh >>= post
  return ()
  where post = Event.post (loop view) . eventFor
        eventFor key = case key of
          Curses.KeyUp     -> Event.Dir DirUp
          Curses.KeyDown   -> Event.Dir DirDown
          Curses.KeyLeft   -> Event.Dir DirLeft
          Curses.KeyRight  -> Event.Dir DirRight
          Curses.KeyChar c -> Event.Key c
          otherwise        -> Event.Noop
