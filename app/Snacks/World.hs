module Snacks.World
( World(size, screen, snake, food, speed)
, Screen(..)
, Snake(dir, body)
, Food(position)
, WorldUpdate(..)
, createWorld
, handleEvent
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
                   , gen    :: StdGen
                   }
  deriving (Show)

data WorldUpdate = SetScreen Screen
                 | MoveSnake Snake Snake
                 | TurnSnake Snake Snake
                 | GrowSnake Snake Snake
                 | NewFood   Food  Food
  deriving (Show)

createWorld :: StdGen -> (Int, Int) -> World
createWorld g s = World { size   = s
                        , screen = Start
                        , snake  = createSnake snakePos
                        , food   = createFood foodPos
                        , speed  = 100
                        , gen    = g''
                        }
  where (snakePos, g') = randomPos g  (quadDimensions s 0)
        (foodPos, g'') = randomPos g' (quadDimensions s 3)

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
  if willEat (food world) (snake world) then do
    let (food', gen') = newFood world
        world'        = world { food = food'
                              , gen  = gen'
                              , snake = growSnake (snake world)
                              }
    tell [ NewFood (food world) (food world')
         , GrowSnake (snake world') (snake world')
         ]
    return world'
  else do
    let world' = world { snake = moveSnake (snake world) }
    tell [MoveSnake (snake world) (snake world')]
    return world'
step (Dir d) world = do
  let world' = world { snake = turnSnake d (snake world) }
  tell [TurnSnake (snake world) (snake world')]
  return world'
step _ world = return world

newFood :: World -> (Food, StdGen)
newFood world = (createFood newPos, gen')
  where headPos        = head . body . snake $ world
        snakeQuad      = posQuad (size world) headPos
        dimensions     = quadDimensions (size world) . oppositeQuad $ snakeQuad
        (newPos, gen') = randomPos (gen world) dimensions

turnSnake :: Direction -> Snake -> Snake
turnSnake d s =
  if d == dir s || d == (opposite . dir $ s)
     then s
     else s { dir = d }

moveSnake :: Snake -> Snake
moveSnake snake = snake { body = n : i }
  where h = head . body $ snake
        i = init . body $ snake
        n = next (dir snake) h

growSnake :: Snake -> Snake
growSnake snake = snake { body = n : (body snake) }
  where h = head . body $ snake
        n = next (dir snake) h

next :: Direction -> (Int, Int) -> (Int, Int)
next DirUp    (row, col) = (row - 1, col)
next DirDown  (row, col) = (row + 1, col)
next DirLeft  (row, col) = (row, col - 1)
next DirRight (row, col) = (row, col + 1)

willEat :: Food -> Snake -> Bool
willEat food snake = position food == next (dir snake) (head . body $ snake)

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
posQuad (sizeR, sizeC) (posR, posC)
  | (posR <  sizeR `div` 2 && posC <  sizeC `div` 2) = 0
  | (posR <  sizeR `div` 2 && posC >= sizeC `div` 2) = 1
  | (posR >= sizeR `div` 2 && posC <  sizeC `div` 2) = 2
  | (posR >= sizeR `div` 2 && posC >= sizeC `div` 2) = 3
