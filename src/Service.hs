module Service (makeMT, MTService, query, User) where

import Network.HTTP.Req
import Data.Aeson
import Data.Text hiding (strip, length, all)
import Control.Monad.IO.Class
import Data.Maybe (maybe)
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.Char as C
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import HTMLEntities.Decoder (htmlEncodedText)
-- import Debug.Trace


type ISOLang = String
type LangPair = (ISOLang, ISOLang)

type User = String
newtype Chunk = Ck (Text, Text, Text)
    deriving Show

type TranslationResult = Either String Text

data MTService = MT
    { user :: Maybe User
    , pair :: LangPair
    , query :: Text -> IO Text
    }

makeMT :: Maybe User -> String -> String -> MTService
makeMT muser src dst = MT 
    { user = muser
    , pair = pair
    , query = _translate muser pair
    }
    where Just pair = makeLangPair src dst

getTranslation :: Maybe User -> LangPair -> Text -> IO Value
getTranslation muser langs src_text = 
    runReq defaultHttpConfig $ do
        r <- req
                GET
                (https url /: "get")
                NoReqBody
                jsonResponse params'
        return (responseBody r :: Value)
    where
        (src,dst) = langs
        params = 
            "q" =: src_text <>
            "langpair" =: (src ++ "|" ++ dst :: String)
        params' = maybe params (\user -> params <> "de" =: user) muser
        url = "api.mymemory.translated.net"

parseResponse :: Value -> Parser Text
parseResponse = withObject "response" $ \o -> do
    status <- o .: "responseStatus"
    rdata  <- o .: "responseData"
    if status == (200 :: Int)
    then do withObject "data" (.: "translatedText") rdata
    else fail $ "Failed, Reason: " ++ show status

_translate :: Maybe User -> LangPair -> Text -> IO Text
_translate muser pair src_text = 
    if Data.Text.null text
        then pure src_text
        else do
            resp <- getTranslation muser pair text
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

makeLangPair :: String -> String -> Maybe LangPair 
makeLangPair src dst | isISOLang src && isISOLang dst = Just (src,dst)
                     | otherwise  = Nothing
        where 
            isISOLang l = length l == 2 && all C.isLower l && all C.isLetter l
