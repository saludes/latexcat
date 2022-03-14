module LaTeXTranslation where
import Service
import Config (getConfig, Config, commands, environments)
import Text.LaTeX.Base.Parser
import Text.LaTeX hiding (words)
import qualified Text.LaTeX.Base.Syntax as S
import qualified Data.Text as T
import Data.Functor ((<&>))
import qualified Data.Text.IO as TIO
import Control.Monad.State.Lazy
import qualified Data.Char as C
import Text.Regex.TDFA ((=~))
import Text.Regex.Base
import qualified System.ProgressBar as PB
import qualified GHC.Conc.IO as CIO


latexTranslate :: MTService -> Text -> IO Text
latexTranslate mt src_text = 
    case parseLaTeX src_text of
        Left err -> fail $ show err
        Right latex  -> do
            config <- getConfig Nothing
            latex' <- translate config mt latex
            return  $ render latex' 


mustProcess :: Config -> LaTeX ->  Bool
mustProcess config = p where
  mustTranslateCommArgs cmd = cmd `elem` commands config
  mustTranslateEnvContent env = env `notElem` environments config
  p (S.TeXRaw _) = True
  p (S.TeXComm cmd _) = not $ mustTranslateCommArgs cmd
  p (S.TeXEnv env _ _) = not $ mustTranslateEnvContent env
  p (S.TeXMath _ _) = True
  -- p (S.TeXBraces _) = True
  -- p (S.TeXSeq _ _)  = True
  p _               = False


wrap :: Int -> Text
wrap code = T.pack $ "<<" ++ show code ++ ">>"


translate :: Config -> MTService -> LaTeX -> IO LaTeX 
translate config service = S.texmapM mp trans
  where
    mp = mustProcess config  
    trans (S.TeXRaw txt) | wordCount txt >= 1 = 
        do
          trans <- query service txt
          pure $ case trans of
            Left code -> S.TeXRaw $ wrap code <> txt
              --S.TeXSeq (S.TeXComment $ "MT code: " <> T.pack (show code)) (S.TeXRaw txt)
            Right  ttxt -> S.TeXRaw ttxt
    trans latex = pure latex


translateMarks :: Config -> MTService -> LaTeX -> IO LaTeX 
translateMarks config service = S.texmapM mp trans
  where
    mp = mustProcess config
    code429 = wrap 429
    trans lt@(S.TeXRaw txt) =
        case T.stripPrefix code429 txt of
          Just txt' -> do
              resp <- query service txt'
              case resp of
                Left code  -> do
                  if code == 429
                    then pure lt -- translation retried but failed
                    else do
                      let  notxt = wrap code <> txt
                      TIO.putStrLn notxt
                      pure $ S.TeXRaw $ code429 <> notxt  -- Translation failed for a different reason! Mark it
                Right ttxt -> do
                  TIO.putStrLn $ "Done: <segment>" <> ttxt <> "</segment>"
                  pure $ S.TeXRaw ttxt   -- Translated
          Nothing -> do
            -- TIO.putStrLn $ "Skipping <segment>" <> txt <> "</segment>"
            pure lt  -- Already done
    trans lt = pure lt

translateMarks_ :: MTService -> Text -> IO LaTeX 
translateMarks_ service txt = 
  case T.stripPrefix caen txt of
    Just txt' -> do
      resp <- query service txt'
      case resp of
        Left code -> do
          putStrLn $ "code: " ++ show code
          pure $ S.TeXRaw txt
        Right ttxt -> do
          TIO.putStrLn $ "translated:" <> ttxt
          pure $ S.TeXRaw ttxt
    Nothing -> pure $ S.TeXRaw txt
  where 
    caen = "<<ca:en>>"
          


  
        
--
-- Preprocessing
--
putTranslationMarks :: Config -> LangPair -> LaTeX -> LaTeX 
putTranslationMarks config (src,dst) =  S.texmap mp mark
  where
    mp = mustProcess config
    label = T.pack $ "<<" ++ src ++ ":" ++ dst ++ ">>"
    mark (S.TeXRaw txt) | wordCount txt > 1 =
      S.TeXRaw $ 
        case T.stripPrefix label txt of
          Just _  -> txt
          Nothing -> label <> txt
    mark latex = latex

--
-- Postprocessing
--

fixQuotes :: Config -> LaTeX -> LaTeX 
fixQuotes config = S.texmap mp fixq
  where
    mp = mustProcess config
    fixq (S.TeXRaw txt) = S.TeXRaw $ _fixQuotes txt
    fixq latex = latex
    _fixQuotes :: Text -> Text
    _fixQuotes = T.pack . matchQ [] . T.unpack
      where
        matchQ txt0 txt = 
            if i >= 0
              then
                let
                  (pre, txt') = splitAt i txt
                  (m,rest) = splitAt l txt'
                  m' = [head m] ++ "'" ++ [last m]
                in matchQ (txt0 ++ pre ++ m') rest
            else txt0 ++ txt
          where
            (i,l) =  txt =~ re :: (MatchOffset, MatchLength)
            re =  "(l)'.*\\b([aeiouAEIOUhH])" :: String


countWords :: Config -> LaTeX  -> Int
countWords config latex = execState sumall 0
  where
    mp = mustProcess config
    sumall = S.texmapM mp count latex
    count :: LaTeX -> State Int LaTeX
    count (S.TeXRaw txt) =  do
      n <- get
      put $ n + wordCount txt
      pure S.TeXEmpty
    count _ = pure S.TeXEmpty


withProgress :: (Text -> IO LaTeX) -> Config -> LaTeX -> IO LaTeX
withProgress f config latex = do
  let nwords = countWords config latex
      mp = mustProcess config
  pb <- PB.newProgressBar PB.defStyle 10 (PB.Progress 0 nwords ())
  let fio (S.TeXRaw txt) = do
        let n = wordCount txt
        PB.incProgress pb n
        f txt 
      fio lt = pure lt
  S.texmapM mp fio latex

mockText :: Text -> IO LaTeX
mockText  txt = case T.stripPrefix caen txt of
    Just txt' -> pure $ S.TeXComment  "replaced"
    Nothing -> pure $ S.TeXRaw txt
  where 
    caen = "<<ca:en>>"
  
  




wordCount :: Text -> Int
wordCount = length . filter (all isUAlpha) . words . T.unpack where
  other :: [C.Char]
  other = "'·àèéíóòúïüç"
  isUAlpha c = C.isAlpha c || C.toLower c `elem` other


getMark :: Text -> (Maybe LangPair, Text)
getMark txt | txt =~ mark_ = 
  case txt =~ mark_ :: (Text, Text, Text, [Text]) of
    ("", _, post, [src,dst]) -> (Just (T.unpack src, T.unpack dst), post)
    _                  -> (Nothing, txt)
  where
    lang_ = "([a-z]{2})"
    mark_ = "<<" ++ lang_ ++ ":" ++ lang_ ++ ">>"
getMark txt = (Nothing, txt)



main :: IO ()
main = do
  let dir = "/Users/saludes/Desktop/AgustíR/HistoriaGeoDiff/"
      srcFile = "hgd_caen_utf8.tex"
  -- let langs = ("ca","en")
  config <- getConfig Nothing
  txt <- TIO.readFile $ dir ++ srcFile
  let Right latex = parseLaTeX txt
      preamble = S.getPreamble latex
      Just body = S.getBody latex
      -- nwords = countWords config body
  -- putStrLn $ "words: " ++ show nwords
  let user = Just "jordi.saludes@upc.edu"
      mt = makeMT user "ca" "en"
      wasteTime t = \latex -> do
        CIO.threadDelay (t * 1000)  -- ms
        pure S.TeXEmpty
  body' <- withProgress (translateMarks_ mt) config body
  {-- body' <- translateMarks config mt body
  -- let body' = fixQuotes body
  -- let body' = putTranslationMarks config langs body -}
  let latex' = preamble <> document body'
      dstFile = dir ++ srcFile ++ ".en.tex"
  TIO.writeFile dstFile . render $ latex'
      -- runStateT (firstWords latex) 0 <&> fst
      -- (lt1, lt2) = split 100 body
  --return (pre, body) -- <> lt1, pre <> lt2) -}