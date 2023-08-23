module Utilities.Io where

import           System.IO

-- TODO: use byte string
writeListToFile :: Show a => FilePath -> a -> IO ()
writeListToFile filename input = do
  handle <- openFile filename WriteMode
  hPrint handle input
  hClose handle
