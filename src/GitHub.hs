module GitHub where

import Prelude hiding ((++))

import Control.Lens
import Network.Wreq hiding (postWith)
import Network.Wreq.Session

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C (unpack)
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H

(++) :: Monoid m => m -> m -> m
(++) = mappend

data CreateFailure
    = AuthFail
    | DecodeFail
    | NotObject
    | MissingField Text
    | FieldTypeMismatch
    | CreateFailure Int
    deriving (Show)

createRelease
    :: Session
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO (Either CreateFailure (Response LB.ByteString))
createRelease session token owner repo tagName =
    resp >>= \ r -> return $ case r ^. responseStatus . statusCode of
        401 -> Left AuthFail
        _ -> Right r
  where
    resp = postWith opts session uri content
    opts = defaults
        & header "Authorization" .~ ["token " ++ token]
        & checkStatus .~ Nothing
    uri = C.unpack $ mconcat
        ["https://api.github.com/repos/", owner, "/", repo, "/releases"]
    content = mconcat
        [ "{\"tag_name\": \"", tagName, "\",\"name\":\"", tagName
        , "\",\"draft\":true}"
        ]

lookupField
    :: Text
    -> Response LB.ByteString
    -> Either CreateFailure Text
lookupField field r = case r ^. responseStatus . statusCode of
    201 -> case A.decode (r ^. responseBody) of
        Nothing -> Left DecodeFail
        Just v -> case v of
            A.Object obj -> case field `H.lookup` obj of
                Nothing -> Left (MissingField field)
                Just v2 -> case v2 of
                    A.String url -> Right url
                    _ -> Left FieldTypeMismatch
            _ -> Left NotObject
    401 -> Left AuthFail
    x -> Left (CreateFailure x)

uploadURL :: ByteString -> Text -> String
uploadURL name t = C.unpack $ mconcat
    [T.encodeUtf8 $ T.takeWhile (/= '{') t, "?name=", name]
