module GitHub where

import Control.Lens
import Network.Wreq

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB

basic :: SB.ByteString -> IO (Response LB.ByteString)
basic token = getWith opts "https://api.github.com/"
  where
    opts = defaults & header "Authorization" .~ [token]
