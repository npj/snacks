module Snacks.Event
( Loop
, Event(..)
, createLoop
, post
, next
) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)

data Event = Up | Down | Left | Right | Key Char | Stop | None
newtype Loop = Loop { channel :: Chan Event }

createLoop :: IO (Loop)
createLoop = Loop <$> newChan

post :: Loop -> Event -> IO ()
post = writeChan . channel

next :: Loop -> IO (Event)
next = readChan . channel

