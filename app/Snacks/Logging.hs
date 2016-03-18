module Snacks.Logging
( Logger
, createLogger
, closeLogger
, logStr
) where

import System.IO (
  IOMode(AppendMode), Handle, openFile, hClose, hPutStrLn,
  hFlush
  )

newtype Logger = Logger { handle :: Handle }

createLogger :: FilePath -> IO Logger
createLogger path = Logger <$> (openFile path AppendMode)

closeLogger :: Logger -> IO ()
closeLogger = hClose . handle

logStr :: String -> Logger -> IO ()
logStr str logger = hPutStrLn h str >> hFlush h
  where h = handle logger
