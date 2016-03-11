module Snacks.Event
( Loop
, Event(..)
, createLoop
, post
, next
) where

import Snacks.Types (Direction)

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)

data Event = Dir Direction | Key Char | Stop | Tick | Noop
  deriving (Eq, Show)
newtype Loop = Loop { channel :: Chan Event }

createLoop :: IO (Loop)
createLoop = Loop <$> newChan

post :: Loop -> Event -> IO ()
post = writeChan . channel

next :: Loop -> IO (Event)
next = readChan . channel

