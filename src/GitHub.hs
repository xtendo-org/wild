module GitHub where

import Control.Lens
import Network.Wreq

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB

createRelease
    :: ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO (Response LB.ByteString)
createRelease token owner repo tagName = postWith opts uri content
  where
    opts = defaults & header "Authorization" .~ ["token " `mappend` token]
    uri = C.unpack $ mconcat
        ["https://api.github.com/repos/", owner, "/", repo, "/releases"]
    content = mconcat
        [ "{\"tag_name\": \"", tagName, "\",\"name\":\"", tagName
        , "\",\"draft\":true}"]
