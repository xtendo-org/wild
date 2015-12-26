module GitHub where

import Prelude hiding ((++))

import Control.Lens
import Network.Wreq hiding (postWith)
import Network.Wreq.Session

import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H

import ByteString (ByteString, RawFilePath)
import qualified ByteString as B

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
    uri = B.unpack $ mconcat
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

uploadAsset
    :: Session
    -> ByteString
    -> RawFilePath
    -> Text
    -> IO (Response LB.ByteString)
uploadAsset session token path url = B.readFile path >>=
    postWith opts session uploadURL
  where
    opts = defaults
        & header "Authorization" .~ ["token " ++ token]
        & checkStatus .~ Nothing
    name = snd $ B.breakEnd (== '/') path
    uploadURL = B.unpack $ mconcat
        [T.encodeUtf8 $ T.takeWhile (/= '{') url, "?name=", name]

uploadRelease
    :: ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> RawFilePath
    -> IO (Either CreateFailure (Response LB.ByteString))
uploadRelease token owner repo tagName path = withSession $ \ s -> do
    r1 <- createRelease s token owner repo tagName
    case r1 >>= lookupField "upload_url" of
        Left err -> return $ Left err
        Right t -> do
            r2 <- uploadAsset s token path t
            return $ Right r2
