module Utilities.Io where

-- import           System.IO
import qualified Data.ByteString.Lazy.Char8 as BLC

-- TODO: use byte string
-- writeListToFile :: Show a => FilePath -> [a] -> IO ()
-- writeListToFile path list = do
--   handle <- openFile path WriteMode
--   mapM_ (hPrint handle) list
--   hClose handle

writeListToFile :: Show a => FilePath -> [a] -> IO ()
writeListToFile path list = BLC.writeFile path content
  where
    content = BLC.unlines . map (BLC.pack . show) $ list
