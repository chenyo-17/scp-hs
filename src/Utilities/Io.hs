module Utilities.Io where

import           System.IO

-- TODO: use byte string
writeListToFile :: Show a => FilePath -> [a] -> IO ()
writeListToFile path list = do
  handle <- openFile path WriteMode
  mapM_ (hPrint handle) list
  hClose handle
