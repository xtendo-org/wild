module Platform where

import Data.ByteString (ByteString)

import RawFilePath

stripCommand :: ByteString -> IO ()
stripCommand bin = run "strip"
    [ "--strip-all"
    , "--remove-section=.comment", "--remove-section=.note"
    , bin
    ]
