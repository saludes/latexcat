module LaTeXTranslation where
import Service
import Text.LaTeX.Base.Parser
import Text.LaTeX hiding (words)
import qualified Text.LaTeX.Base.Syntax as S
import qualified Data.Text as T
import Data.Functor ((<&>))
-- import Data.Semigroup (Sum (..))

import qualified Data.Text.IO as TIO
import  Control.Monad.State.Lazy
import qualified Data.Char as C
import Text.Regex.TDFA ((=~))
import Text.Regex.Base


latexTranslate :: MTService -> Text -> IO Text
latexTranslate mt src_text = 
    case parseLaTeX src_text of
        Left err -> fail $ show err
        Right latex  -> do
            latex' <- translate mt latex
            return  $ render latex'


mustProcess :: LaTeX -> Bool
mustProcess = p where
  p (S.TeXRaw _) = True
  p (S.TeXComm cmd _) = not $ mustTranslateCommArgs cmd
  p (S.TeXEnv env _ _) = not $ mustTranslateEnvContent env
  p (S.TeXMath _ _) = True
  -- p (S.TeXBraces _) = True
  -- p (S.TeXSeq _ _)  = True
  p _               = False

mustTranslateCommArgs, mustTranslateEnvContent :: String -> Bool 
mustTranslateEnvContent = not . (`elem` tEnvs) where
  tEnvs = words $ mathEnvs ++ graphEnvs
  mathEnvs = " equation align "
  graphEnvs = " wrapfigure figure"

mustTranslateCommArgs = (`elem` tCmds) where
  tCmds = words $ styleCmds ++ sectionCmds ++ otherCmds
  styleCmds = " emph it "
  sectionCmds = " title chapter section subsection subsubsection "
  otherCmds = " footnote caption "


wrap :: Int -> Text
wrap code = T.pack $ "<<" ++ show code ++ ">>"


translate :: MTService -> LaTeX -> IO LaTeX 
translate service = S.texmapM mustProcess trans where
  trans (S.TeXRaw txt) | wordCount txt >= 1 = 
    do
      trans <- query service txt
      pure $ case trans of
        Left code -> S.TeXRaw $ wrap code <> txt
          --S.TeXSeq (S.TeXComment $ "MT code: " <> T.pack (show code)) (S.TeXRaw txt)
        Right  ttxt -> S.TeXRaw ttxt
  trans latex = pure latex

recoverTranslate :: MTService -> LaTeX -> IO LaTeX 
recoverTranslate service = S.texmapM mustProcess trans where
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
        
--
-- Preprocessing
--
putTranslationMarks :: LangPair -> LaTeX -> LaTeX 
putTranslationMarks (src,dst) = S.texmap mustProcess mark
  where
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

fixQuotes :: LaTeX -> LaTeX 
fixQuotes = S.texmap mustProcess fixq where
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


countWords :: LaTeX  -> Int
countWords latex = execState sumall 0
  where
    sumall = S.texmapM mustProcess count latex
    count :: LaTeX -> State Int LaTeX
    count (S.TeXRaw txt) =  do
      n <- get
      put $ n + wordCount txt
      pure S.TeXEmpty
    count _ = pure S.TeXEmpty 

wordCount :: Text -> Int
wordCount = length . filter (all isUAlpha) . words . T.unpack where
  other :: [C.Char]
  other = "'·àèéíóòúïüç"
  isUAlpha c = C.isAlpha c || C.toLower c `elem` other 


main :: IO ()
main = do
  let dir = "omega-target/"
  txt <- TIO.readFile $ dir ++ "30.tex"
  let Right latex = parseLaTeX txt
      pre = S.getPreamble latex
      Just body = S.getBody latex
      {-user = Just "jordi.saludes@upc.edu"
      mt = makeMT user "ca" "en" 
  body' <- recoverTranslate mt body -}
  -- let body' = fixQuotes body
  putStrLn $ "words: " ++ show (countWords body)
  let body' = putTranslationMarks ("en", "ca") body
  let latex' = pre <> document body'
  TIO.writeFile (dir ++ "30-other.tex") . render $ latex'
      -- runStateT (firstWords latex) 0 <&> fst
      -- (lt1, lt2) = split 100 body
  --return (pre, body) -- <> lt1, pre <> lt2)