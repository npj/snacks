module Snacks
( run
) where

import qualified Snacks.Event as Event
import qualified Snacks.UI as UI
import qualified Snacks.Game as Game

import Control.Exception (finally)

-- View: gets input events from the user and posts events to the event loop
-- Engine: handles events on the event loop and updates the view
run :: IO ()
run = do
  loop   <- Event.createLoop
  view   <- UI.createView loop
  engine <- Game.createEngine loop view
  Game.runEngine engine `finally` UI.destroyView view
