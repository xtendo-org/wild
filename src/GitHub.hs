module GitHub where

import Control.Lens
import Network.Wreq

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H

data CreateFailure
    = AuthFail
    | DecodeFail
    | NotObject
    | MissingField Text
    | FieldTypeMismatch
    | CreateFailure Int
    deriving (Show)

createRelease
    :: ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO (Either CreateFailure (Response LB.ByteString))
createRelease token owner repo tagName =
    resp >>= \ r -> return $ case r ^. responseStatus ^. statusCode of
        401 -> Left AuthFail
        _ -> Right r
  where
    resp = postWith opts uri content
    opts = defaults
        & header "Authorization" .~ ["token " `mappend` token]
        & checkStatus .~ Nothing
    uri = C.unpack $ mconcat
        ["https://api.github.com/repos/", owner, "/", repo, "/releases"]
    content = mconcat
        [ "{\"tag_name\": \"", tagName, "\",\"name\":\"", tagName
        , "\",\"draft\":true}"]

lookupField
    :: Text
    -> Response LB.ByteString
    -> Either CreateFailure Text
lookupField field r = case r ^. responseStatus ^. statusCode of
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
