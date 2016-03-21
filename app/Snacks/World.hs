module Snacks.World
( World(size, screen, snake, food, speed)
, Screen(..)
, Snake(dir, body)
, Food(position)
, WorldUpdate(..)
, createWorld
, handleEvent
, newFood
, moveSnake
, turnSnake
, growSnake
, snakeAteFood
) where

import Snacks.Types (
    Direction(DirUp, DirDown, DirLeft, DirRight)
  , opposite
  )

import Snacks.Event (Event(..))

import System.Random (StdGen, randomR)
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)

data Screen = Start | PrePlay | Playing
  deriving (Show, Read, Eq)

data Snake = Snake { dir  :: Direction
                   , body :: [(Int, Int)]
                   }
  deriving (Show)

data Food = Food { position :: (Int, Int) }
  deriving (Show)

data World = World { size   :: (Int, Int)
                   , screen :: Screen
                   , snake  :: Snake
                   , food   :: Food
                   , speed  :: Float
                   }
  deriving (Show)

data WorldUpdate = SetScreen Screen
                 | MoveSnake Snake Snake
                 | TurnSnake Snake Snake
  deriving (Show)

createWorld :: StdGen -> (Int, Int) -> World
createWorld gen s = World { size   = s
                          , screen = Start
                          , snake  = createSnake snakePos
                          , food   = createFood foodPos
                          , speed  = 100
                          }
  where (snakePos, gen') = randomPos gen  (quadDimensions s 0)
        (foodPos, _)     = randomPos gen' (quadDimensions s 3)

createFood :: (Int, Int) -> Food
createFood pos = Food { position = pos }

createSnake :: (Int, Int) -> Snake
createSnake pos = Snake { dir  = DirRight
                        , body = [pos]
                        }

handleEvent :: Event -> World -> (World, [WorldUpdate])
handleEvent event world = runWriter $ handle (screen world) event world
  where handle Start   = start
        handle PrePlay = play
        handle Playing = step

start :: Event -> World -> Writer [WorldUpdate] World
start _ world = do
  tell [SetScreen PrePlay]
  return $ world { screen = PrePlay }

play :: Event -> World -> Writer [WorldUpdate] World
play (Dir d) world = do
  let world' = world { snake  = turnSnake d (snake world)
                     , screen = Playing
                     }
  tell [TurnSnake (snake world) (snake world'), SetScreen Playing]
  return world'
play _ world = return world

step :: Event -> World -> Writer [WorldUpdate] World
step Tick world = do
  let world' = world { snake = moveSnake (snake world) }
  tell [MoveSnake (snake world) (snake world')]
  return world'
step (Dir d) world = do
  let world' = world { snake = turnSnake d (snake world) }
  tell [TurnSnake (snake world) (snake world')]
  return world'
step _ world = return world

newFood :: StdGen -> World -> World
newFood gen w = w { food = createFood . fst . randomPos gen $ dimensions }
  where headPos    = head . body . snake $ w
        snakeQuad  = posQuad (size w) headPos
        dimensions = quadDimensions (size w) . oppositeQuad $ snakeQuad

turnSnake :: Direction -> Snake -> Snake
turnSnake d s =
  if d == dir s || d == (opposite . dir $ s)
     then s
     else s { dir = d }

moveSnake :: Snake -> Snake
moveSnake s = s { body = next (dir s) (head . body $ s) : (tail . body $ s) }
  where next DirUp    (row, col) = (row - 1, col)
        next DirDown  (row, col) = (row + 1, col)
        next DirLeft  (row, col) = (row, col - 1)
        next DirRight (row, col) = (row, col + 1)

growSnake :: Snake -> Snake
growSnake = undefined

snakeAteFood :: Snake -> Food -> Bool
snakeAteFood = undefined

randomPos :: StdGen -> ((Int, Int, Int, Int)) -> ((Int, Int), StdGen)
randomPos gen (r, c, rows, cols) = ((r + row, c + col), gen'')
  where (row, gen')  = randomR (1, rows - 1) gen
        (col, gen'') = randomR (1, cols - 1) gen'

quadDimensions :: (Int, Int) -> Int -> ((Int, Int, Int, Int))
quadDimensions (rows, cols) i = case i of
  0 -> (0, 0, r, c)
  1 -> (0, c, r, c)
  2 -> (r, 0, r, c)
  3 -> (r, c, r, c)
  where r = rows `div` 2
        c = cols `div` 2

oppositeQuad :: Int -> Int
oppositeQuad 0 = 3
oppositeQuad 1 = 2
oppositeQuad 2 = 1
oppositeQuad 3 = 0

posQuad :: (Int, Int) -> (Int, Int) -> Int
posQuad (sizeR, sizeC) (posR, posC) = undefined
