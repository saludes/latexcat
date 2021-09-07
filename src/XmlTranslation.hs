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
                pure $ showTopElement root'
        _         -> error "parsing failed."
    where
        sq s = do
            t <- query mt $ T.pack s
            return $ T.unpack  t


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
        config = TS.renderOptions { TS.optEscape = id} :: TS.RenderOptions XMLString
        render = TS.renderTagsOptions config
        

main :: IO ()
main = do
    contents <- readFile test_in_path
    contents' <- xmlTranslate mt contents
    -- let contents' = showTopElement root'
    writeFile test_out_path $ decodeHTML contents'
    where
        lang1 = "es"
        lang2 = "ca"
        user = "jordi.saludes@upc.edu"
        mt = makeMT  (Just user) lang1  lang2
        test_in_path = "test/samples/fourier-series.xml"
        test_out_path = test_in_path ++ "." ++ lang2

