module Platform where

import Data.ByteString (ByteString)

import RawFilePath

stripCommand :: ByteString -> IO ()
stripCommand bin = run "strip" ["-u", "-r", bin]
