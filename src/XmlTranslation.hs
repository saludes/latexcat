module XmlTranslation where
import Text.XML.Light
import qualified Text.HTML.TagSoup as TS

import qualified Data.Text as T
import Service

type Transformation = String  -> IO String 
type XMLString = String

xmlTranslate :: MTService -> XMLString -> IO XMLString
xmlTranslate mt src_text = do
    let ppc = ppcElement defaultConfigPP
    case parseXMLDoc src_text of
        Just root -> do
                root' <- transform sq root
                pure $ decodeHTML $ showTopElement root'
        _         -> error "parsing failed."
    where
        sq s = do
            resp <- query mt ("es","ca") $ T.pack s -- TODO: language must not be constant
            case resp of
                Left code -> error $ "Service throw error " ++ show code
                Right txt -> return $ T.unpack txt

xmlMark :: LangPair -> XMLString -> XMLString
xmlMark lp srcTxt = 
    case parseXMLDoc srcTxt of
        Just root -> decodeHTML . showTopElement $ mark lp root
        _         -> error "parsing failed"

addLangPair :: LangPair -> Element -> Element 
addLangPair lp el = 
    case filter ((==key).attrKey) $ elAttribs el of
        [] -> add_attr langAtr el
        [atr] -> if attrVal atr == value
            then el
            else error $ "Already marked as " ++ attrVal atr
        _     -> error "Many lang marks"
    where
        (src,dest) = lp
        key = QName "lang" Nothing (Just "mt")
        value = src ++ ":" ++ dest
        langAtr = Attr key value

mark :: LangPair -> Element  -> Element 
mark lp el = 
    if isSkipElem el
        then el
        else 
            addLangPair lp $ el {elContent = map markContent $ elContent el}
    where 
        markContent (Elem el) = Elem $ mark lp el
        markContent cnt       = cnt



transform :: Transformation -> Element -> IO Element
transform trans el =
        if isSkipElem el
        then pure el
        else do
            contents' <- mapM transformContent $ elContent el
            pure $ el {elContent = contents'}
    where
        transformContent (Elem el) = Elem <$> transform trans el
        transformContent (Text cdata) = do
            let cdata' = cdData cdata
            cdata'' <- trans cdata'
            return $ Text cdata {cdData = cdata'' }
            
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
    xml <- readFile test_in_path
    case parseXMLDoc xml of
        Just root -> do
            root' <- transform pure root
            let contents' = decodeHTML $ showTopElement root'
            writeFile test_out_path  contents'
        _         -> error "parsing failed."
    
    where
        test_in_path = "test/samples/fourier-series.xml"
        test_out_path = test_in_path ++ ".out"

