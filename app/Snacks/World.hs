module Snacks.World
( World(wSize, wScreen, wSnake, wFood)
, Screen(..)
, Snake
, Food
, createWorld
, createFood
, createSnake
) where

import System.Random (StdGen)

data Screen = Start | PreStart | NoScreen
  deriving (Show, Read, Eq)

data Snake = Snake
data Food = Food

data World = World { wSize   :: (Int, Int)
                   , wScreen :: Screen
                   , wSnake  :: Snake
                   , wFood   :: Food
                   }

createWorld :: (Int, Int) -> World
createWorld s = World { wSize   = s
                      , wScreen = NoScreen
                      , wSnake  = Snake
                      , wFood   = Food
                      }

createFood :: StdGen -> Food
createFood = undefined

createSnake :: StdGen -> Snake
createSnake = undefined

