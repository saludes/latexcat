module Service (
    makeMT, 
    MTService,
    makeLangPair,
    query, pair,
    User,
    ISOLang, LangPair) where

import Network.HTTP.Req
-- import qualified Network.HTTP.Client as NC -- for proxy
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
    , query :: Text -> IO (Either Int Text)
    }

instance Show MTService where
    show (MT user (l0,l1) _) = maybe s (\u -> s ++ " for " ++ u) user ++ ")"
        where s = "(MT service " ++ l0 ++ ">" ++ l1 

makeMT :: Maybe User -> String -> String -> MTService
makeMT muser src dst = MT 
    { user = muser
    , pair = pair
    , query = _translate muser pair
    }
    where Just pair = makeLangPair src dst


getTranslation :: Maybe User -> LangPair -> Text -> IO (Either Int Value)
getTranslation muser langs src_text = 
    runReq httpConfig $ do
        resp <- req
                GET
                (https url /: "get")
                NoReqBody
                jsonResponse params'
        return $ case responseStatusCode resp of
            200 ->  Right (responseBody resp :: Value)
            n   -> Left n 
    where
        httpConfig = defaultHttpConfig { 
            httpConfigCheckResponse = \_ _ _ -> Nothing
            -- , httpConfigProxy = Just $ NC.Proxy "127.0.0.1" 8080
        }
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

_translate :: Maybe User -> LangPair -> Text -> IO (Either Int Text)
_translate muser pair src_text = 
    if Data.Text.null text
        then pure $ Right src_text
        else do
            resp <- getTranslation muser pair text
            case resp of
                Left code -> pure $ Left code
                Right r -> 
                    case parse parseResponse r of 
                        Success txt -> do
                            let nw = Ck (pre, htmlDecode txt,suff)
                            pure $ Right $ unstrip nw 
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
