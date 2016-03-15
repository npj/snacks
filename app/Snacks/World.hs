module Snacks.World
( World(size, screen, snake, food, speed)
, Screen(..)
, Snake(dir, body)
, Food(position)
, createWorld
, moveSnake
, turnSnake
) where

import Snacks.Types (
    Direction(DirUp, DirDown, DirLeft, DirRight)
  , opposite
  )

import System.Random (StdGen, randomR)

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

createWorld :: StdGen -> (Int, Int) -> World
createWorld gen s = World { size   = s
                          , screen = Start
                          , snake  = createSnake snakePos
                          , food   = createFood foodPos
                          , speed  = 100
                          }
  where (snakePos, gen') = randomPos gen  (quadrant 0 s)
        (foodPos, _)     = randomPos gen' (quadrant 3 s)

createFood :: (Int, Int) -> Food
createFood pos = Food { position = pos }

createSnake :: (Int, Int) -> Snake
createSnake pos = Snake { dir  = DirRight
                        , body = [pos]
                        }

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

randomPos :: StdGen -> ((Int, Int, Int, Int)) -> ((Int, Int), StdGen)
randomPos gen (r, c, rows, cols) = ((r + row, c + col), gen'')
  where (row, gen')  = randomR (1, rows - 1) gen
        (col, gen'') = randomR (1, cols - 1) gen'

quadrant :: Int -> (Int, Int) -> ((Int, Int, Int, Int))
quadrant i (rows, cols) = case i of
  0 -> (0, 0, r, c)
  1 -> (0, c, r, c)
  2 -> (r, 0, r, c)
  3 -> (r, c, r, c)
  where r = rows `div` 2
        c = cols `div` 2
