module LaTeXTranslation where
import Service
import Config (getConfig, Config, commands, environments)
import Text.LaTeX.Base.Parser
import Text.LaTeX hiding (words)
import qualified Text.LaTeX.Base.Syntax as S
import qualified Data.Text as T
import Data.Functor ((<&>))
import qualified Data.Text.IO as TIO
import qualified Data.Char as C
import Text.Regex.TDFA ((=~))
import Text.Regex.Base
import qualified System.ProgressBar as PB
import qualified GHC.Conc.IO as CIO
import  Control.Monad.Trans.State.Lazy



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

data Segment 
    = Translate LangPair Text
    | Done Text
  deriving Show

putSegment :: Segment -> Text
getSegment :: Text -> Segment
getSegment txt = 
      case txt =~ re of
        [[pre, l1,l2]] -> Translate (up l1, up l2) (T.drop (T.length pre) txt)
        _              -> Done txt
      where re =  "<<([a-z]{2}):([a-z]{2})>>" :: Text
            up = T.unpack

putSegment (Done txt) = txt
putSegment (Translate lpair txt) = wrap lpair <> txt
  where
    wrap :: LangPair -> Text  
    wrap (l1,l2) = T.pack $ "<<" ++ l1 ++ ":" ++ l2 ++ ">>"


translateLaTeX :: Config -> MTService -> LaTeX -> IO LaTeX 
translateLaTeX cfg service latex = do
    (latex', notDone) <- runStateT transM False
    putStrLn $ if notDone then "Still." else "Finished!"
    return latex'
  where
    trans :: LaTeX -> StateT Bool IO LaTeX
    trans (S.TeXRaw txt) = translateSegment service (getSegment txt) <&> S.TeXRaw . putSegment
    trans latex = pure latex
    transM = S.texmapM mp trans latex
    mp  = mustProcess cfg

translateLaTeXDoc :: Config -> MTService  -> Text -> IO Text
translateLaTeXDoc cfg service doc = do
    let Right latex = parseLaTeX doc
        preamble  = S.getPreamble latex
        Just body = S.getBody latex
    body' <- translateLaTeX cfg service body
    return $ render (preamble <> body')
  

translateSegment :: MTService -> Segment -> StateT Bool IO Segment
translateSegment service seg@(Translate lpair srcTxt) = do
  has429 <- get
  if has429
    then do -- Already failed: skip
      lift $ putStr " "
      pure seg 
    else do -- try to translate
      resp <- lift $ query service lpair srcTxt
      case resp of
        Left code | code == 429 -- limit reached now
            -> lift (putStr "!") >> put True >> pure seg 
        Left code  -- other code
            -> error $ "Got error: " ++ show code
        Right dstTxt -- success
            -> lift (putStr ".") >> pure (Done dstTxt)
translateSegment _ seg = pure seg


  
        
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

count :: (LaTeX -> Int) -> Config -> LaTeX -> Int
count f cfg latex  = execState scnt 0
  where
    mp = mustProcess cfg
    scnt = S.texmapM mp fcnt latex
    fcnt :: LaTeX -> State Int LaTeX
    fcnt latex = do
      n <- get
      put $ n + f latex
      pure S.TeXEmpty 




countWords, countRemainingWords :: Config -> LaTeX  -> Int
countSegments, countRemainingSegments :: Config -> LaTeX -> Int
countWords = count wordCnt
  where
    wordCnt (S.TeXRaw txt) = wordCount txt
    wordCnt _              = 0
    
countRemainingWords = count remCnt
  where
    remCnt (S.TeXRaw txt) =
      case getSegment txt of
        Translate ("ca", "en") txt -> wordCount txt
        Translate _ _              -> error "Must be ca > en"
        _                          -> 0
    remCnt _              = 0

countSegments = count segCnt
  where
    segCnt (S.TeXRaw txt) = 1
    segCnt _              = 0

countRemainingSegments = count segCnt
  where
    segCnt (S.TeXRaw txt) =
      case getSegment txt of
        Translate ("ca","en") _ -> 1
        Translate _ _           -> error "Must be ca > en"
        Done _                  -> 0
    segCnt _              = 0


getFirstSegment :: Config -> LaTeX -> (Int, Segment)
getFirstSegment cfg latex =
  case execState (S.texmapM mp getf latex) (0,Nothing) of
    (nsegs, Just seg) -> (nsegs, seg) 
    _        -> error "Cant find segment"
  where
    mp = mustProcess cfg
    getf :: LaTeX -> State (Int, Maybe Segment) LaTeX
    getf (S.TeXRaw txt) = do
      (nsegs, ms) <- get
      case ms of
        Nothing -> case getSegment txt of
                    Done _ -> put (nsegs +1, Nothing)
                    seg    -> put (nsegs, Just seg)
        _       -> return ()
      pure S.TeXEmpty 
    getf _              = pure S.TeXEmpty 


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


remaining :: IO () 
remaining = do
  let dir = "/Users/saludes/Desktop/AgustíR/HistoriaGeoDiff/"
      srcFile = "hgd_caen_utf8.tex"
  cfg <- getConfig Nothing
  txt <- TIO.readFile $ dir ++ srcFile
  let Right latex = parseLaTeX txt
      preamble = S.getPreamble latex
      Just body = S.getBody latex
      totalWords = fromIntegral . countWords cfg $ body :: Float
      wordsToGo  = fromIntegral . countRemainingWords cfg $ body :: Float
      totalSegs = countSegments cfg body
      remSegs = countRemainingSegments cfg body
      done = 1.0 - wordsToGo/totalWords
      --seg = getFirstSegment cfg body
  putStrLn  $ "Done " ++ show (round (100.0 * done)) ++ " %"
  putStrLn $ show remSegs ++ " remaining from a total of " ++ show totalSegs
  --print seg


main :: IO ()
main = do
  let dir = "/Users/saludes/Desktop/AgustíR/HistoriaGeoDiff/"
      srcFile = "hgd_caen_utf8.tex"
      dstFile = dir ++ srcFile ++ ".marked"
      user = Just "jordi.saludes@upc.edu"
      mt = makeMT user
  cfg <- getConfig Nothing
  doc <- TIO.readFile $ dir ++ srcFile
  doc' <- translateLaTeXDoc cfg mt doc
  TIO.writeFile dstFile doc'