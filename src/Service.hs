module Service where

import Network.HTTP.Req
import Data.Aeson
import Data.Text hiding (strip)
import Control.Monad.IO.Class
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.Char as C
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import HTMLEntities.Decoder (htmlEncodedText)
-- import Debug.Trace



type LangPair = (String, String)
type User = String
newtype Chunk = Ck (Text, Text, Text)
    deriving Show

type TranslationResult = Either String Text

data Config = Cfg
    { user :: Maybe User
    , pair :: LangPair }

makeConfig :: String -> String -> Config
makeUserConfig :: User -> String -> String -> Config
makeConfig src dst = Cfg Nothing (src,dst)
makeUserConfig user src dst = Cfg (Just user) (src,dst)

getTranslation :: Config -> Text -> IO Value
getTranslation config src_text = 
    runReq defaultHttpConfig $ do
        r <- req
                GET
                (https url /: "get")
                NoReqBody
                jsonResponse params'
        return (responseBody r :: Value)
    where
        (src,dst) = pair config
        params = 
            "q" =: src_text <>
            "langpair" =: (src ++ "|" ++ dst :: String)
        params' = case user config of
            Just user -> params <> "de" =: user
            Nothing   -> params
        url = "api.mymemory.translated.net"

parseResponse :: Value -> Parser Text
parseResponse = withObject "response" $ \o -> do
    status <- o .: "responseStatus"
    rdata  <- o .: "responseData"
    if status == (200 :: Int)
    then do withObject "data" (.: "translatedText") rdata
    else fail $ "Failed, Reason: " ++ show status

translate :: Config -> Text -> IO Text
translate config src_text = 
    if Data.Text.null text
        then pure src_text
        else do
            resp <- getTranslation config text
            case parse parseResponse resp of 
                Success txt -> do
                    let nw = Ck (pre, htmlDecode txt,suff)
                    pure $ unstrip nw 
                Error err -> fail err 
    where Ck (pre,text,suff) = strip src_text


        
strip :: Text -> Chunk
unstrip :: Chunk -> Text
strip txt =
        let b = Data.Text.break (not . C.isSpace)
            r = Data.Text.reverse
            (prefix, chunk) = b txt
            (rsuffix, rchunk) = b $ r chunk
        in Ck (prefix, r rchunk, r rsuffix)



unstrip (Ck (pre,chunk,suf)) = pre <> chunk <> suf


htmlDecode :: Text -> Text
htmlDecode = toStrict . toLazyText . htmlEncodedText