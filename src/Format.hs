{-# LANGUAGE DeriveGeneric #-}

module Format
    ( FromJSON
    , dec
    , EncodeJSON(enc)
    , Conf(..)
    , initConf
    ) where

import Data.Text
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Encode.Pretty

dec :: FromJSON a => ByteString -> Either String a
dec = eitherDecode . fromStrict

encBase :: ToJSON a => [Text] -> a -> ByteString
encBase keys = (toStrict .) $ encodePretty' $ defConfig
    { confIndent = (Spaces 4)
    , confCompare = keyOrder keys
    }

-- a separate class for explicit ordering of JSON fields
class EncodeJSON a where
    enc :: a -> ByteString

data Conf = Conf
    { owner :: !Text
    , repo :: !Text
    , tokenPath :: !Text
    }
    deriving (Show, Generic)

instance FromJSON Conf
instance ToJSON Conf
instance EncodeJSON Conf where
    enc = encBase ["owner", "repo", "tokenPath"]

initConf :: Conf
initConf = Conf
    "owner"
    "repo"
    "/path/to/tokenfile"
