module Snacks.Types
( Direction(DirUp, DirDown, DirLeft, DirRight)
, opposite
) where

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Eq)

opposite :: Direction -> Direction
opposite DirUp    = DirDown
opposite DirDown  = DirUp
opposite DirLeft  = DirRight
opposite DirRight = DirLeft

