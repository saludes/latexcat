module XmlTranslation where
import Text.XML.Light
import qualified Text.HTML.TagSoup as TS
import Data.Hashable 

import qualified Data.Text as T
import Service

type Transformation = String  -> IO String 
type XMLString = String

langKey :: QName
langKey = QName "langs" Nothing (Just "mt")

xmlTranslate :: Bool -> MTService -> XMLString -> IO XMLString
xmlTranslate sync mt src_text = do
    let ppc = ppcElement defaultConfigPP
    case parseXMLDoc src_text of
        Just root -> do
                root' <- translateElement root
                pure $ decodeHTML $ showTopElement root'
        _         -> error "parsing failed."
    where
        getTranslation :: LangPair -> String -> IO String
        translateElement :: Element -> IO Element
        transformContent :: LangPair -> Content -> IO Content
        getTranslation lp s = do
            resp <- query mt lp $ T.pack s
            case resp of
                Left code -> error $ "Service throw error " ++ show code
                Right txt -> return $ T.unpack txt
        translateElement el =
            if isSkipElem el
            then pure el
            else case getMark el of
                Nothing -> pure el
                Just lp -> do
                    contents' <- mapM (transformContent lp) $ elContent el
                    let el' = rmMark $ el {elContent = contents'}
                    pure el'
        transformContent _ (Elem el) = Elem <$> translateElement el
        transformContent lp (Text cdata) = do
            let cdata' = cdData cdata
            cdata'' <- getTranslation lp cdata'
            pure $ Text cdata {cdData = cdata'' }
        

xmlMark :: LangPair -> XMLString -> XMLString
xmlMark lp srcTxt = 
    case parseXMLDoc srcTxt of
        Just root -> decodeHTML . showTopElement $ addMark lp root
        _         -> error "parsing failed"



addLangPair :: LangPair -> Element -> Element 
addLangPair lp el = 
    case findAttr langKey el of
        Nothing -> add_attr langAtr el
        Just _  -> el
    where
        (src,dest) = lp
        value = src ++ ":" ++ dest
        langAtr = Attr langKey value

getMark :: Element -> Maybe LangPair 
addMark :: LangPair -> Element  -> Element
rmMark :: Element -> Element 
getMark el = 
    case findAttr langKey el of
        Just lp -> case break (==':') lp of
                    (src,':':dst) -> Just (src, dst)
                    _             -> error $ "malformed lang pair: " <> lp
        _       -> Nothing
    
addMark lp el = 
    if isSkipElem el
        then el
        else 
            addLangPair lp $ el {elContent = map markContent $ elContent el}
    where 
        markContent (Elem el) = Elem $ addMark lp el
        markContent cnt       = cnt

rmMark el = el {elAttribs = attrs} where
    attrs = filter ((/=langKey).attrKey) $ elAttribs el
    


addSync :: Element -> Element 
addSync = undefined
            
isSkipElem :: Element -> Bool
isSkipElem = (`elem` avoidTags) . qName . elName where
    avoidTags = Prelude.words "me m mrow "
    

visit :: Element -> IO ()
visit el = do
    print $ elName el
    mapM_ printContent $ elContent el

printContent :: Content  -> IO ()
printContent (Elem el) = visit el
printContent (Text cdata) = printCData cdata
printContent (CRef s) = putStrLn $ "a ref: " ++ s

printCData :: CData  -> IO ()
printCData (CData _ s line) = do
        putStrLn s
        case line of
            Just n -> putStrLn $ "(at line " ++ show n ++ ")"
            _      -> return ()

decodeHTML :: XMLString -> XMLString
decodeHTML  =  render . TS.parseTags
    where
        unesc :: String -> String
        unesc s = 
            case break (`elem` special) s of
                (pre,'&':post) -> pre ++ "&amp;" ++ unesc post
                (pre,'<':post) -> pre ++ "&lt;" ++ unesc post
                (pre,'>':post) -> pre ++ ">" ++ unesc post  -- JP uses '&lt;' but not '&gt;'; why?
                (pre,"")       -> pre
                _              -> error $ "while unescaping: " ++ s
            where
                special :: [Char]
                special = "&<>"
        mini = (`elem` minTags) where
            minTags = words "xi:include image xref video col"
        config = TS.renderOptions
            { TS.optEscape = unesc
            , TS.optMinimize = mini
            } :: TS.RenderOptions XMLString
        render = TS.renderTagsOptions config
        

main :: IO ()
main = do
    let mt = makeMT Nothing
    xml <- readFile test_in_path
    xml' <- xmlTranslate False  mt xml
            -- let contents' = decodeHTML $ showTopElement root'
    writeFile test_out_path  xml'    
    where
        test_in_path = "test/samples/fourier-series.xml"
        test_out_path = test_in_path ++ ".out"

