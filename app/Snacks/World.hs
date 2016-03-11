module Snacks.World
( World(size, screen, snake, food, speed)
, Screen(..)
, Snake(dir)
, Food
, createWorld
) where

import Snacks.Types (Direction(DirRight))

import System.Random (StdGen)

data Screen = Start | PrePlay | Playing
  deriving (Show, Read, Eq)

data Snake = Snake { dir :: Direction }
data Food = Food { position :: (Int, Int) }

data World = World { size   :: (Int, Int)
                   , screen :: Screen
                   , snake  :: Snake
                   , food   :: Food
                   , speed  :: Float
                   }

createWorld :: StdGen -> (Int, Int) -> World
createWorld gen s = World { size   = s
                          , screen = Start
                          , snake  = createSnake gen s
                          , food   = createFood gen s
                          , speed  = 500
                          }

createFood :: StdGen -> (Int, Int) -> Food
createFood _ _ = Food { position = (0, 0) }

createSnake :: StdGen -> (Int, Int) -> Snake
createSnake _ _ = Snake { dir = DirRight }

