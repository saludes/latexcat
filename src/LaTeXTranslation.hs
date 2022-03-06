module LaTeXTranslation where
import Service
import Text.LaTeX.Base.Parser
import Text.LaTeX hiding (words)
import qualified Text.LaTeX.Base.Syntax as S
import qualified Data.Text as T
import Data.Functor ((<&>))
import Data.Semigroup as S

import qualified Data.Text.IO as TIO
import  Control.Monad.Trans.State
import qualified Data.Char as C


fold :: Monoid b => (Text -> b) -> LaTeX  -> b
fold f = visitLatex where
  visitLatex latex =
    case latex of
      S.TeXRaw txt -> f txt 
      S.TeXComm cmd args -> 
        if mustTranslateCommArgs cmd
          then mconcat $ map visitArg args
          else mempty
      S.TeXCommS _ -> mempty 
      S.TeXEnv env args latex ->
        if mustTranslateEnvContent env
          then visitLatex latex -- TODO: Must visit the arguments too?
          else mempty 
      S.TeXBraces latex -> visitLatex latex
      S.TeXSeq latex1 latex2 ->
        visitLatex latex1 <> visitLatex latex2
      S.TeXMath _ _  -> mempty -- TODO: Must visit \mbox'es inside math? 
      S.TeXComment _ -> mempty -- comments are not considered
      _            -> mempty
  visitArg (S.FixArg ltx) = visitLatex ltx
  visitArg (S.OptArg ltx) = visitLatex ltx
  visitArg (S.MOptArg ltxs) = mconcat $ map visitLatex ltxs
  visitArg (S.SymArg ltx) = visitLatex ltx
  visitArg (S.MSymArg ltxs) = mconcat $ map visitLatex ltxs
  visitArg (S.ParArg ltx) = visitLatex ltx
  visitArg (S.MParArg ltxs) = mconcat $ map visitLatex ltxs

transform :: Monad m => (Text -> m Text) -> LaTeX -> m LaTeX
transform f = transL where
  transL latex =
    case latex of
      S.TeXRaw txt -> f txt <&> S.TeXRaw
      S.TeXComm cmd args -> 
        if mustTranslateCommArgs cmd
          then mapM transA args <&> S.TeXComm cmd
          else return latex
      S.TeXEnv env args latex ->
        if mustTranslateEnvContent env
          then transL latex  <&> S.TeXEnv env args-- TODO: Must visit the arguments too?
          else pure latex 
      S.TeXBraces latex -> transL latex <&> S.TeXBraces
      S.TeXSeq latex1 latex2 -> do
          tl1 <- transL latex1
          tl2 <- transL latex2
          pure $ S.TeXSeq tl1 tl2
      S.TeXMath _ _  -> pure latex -- TODO: Must visit \mbox'es inside math? 
      S.TeXComment _ -> pure latex -- comments are not considered
      _              -> pure latex
  transA (S.FixArg ltx) = transL ltx <&> S.FixArg
  transA (S.OptArg ltx) = transL ltx <&> S.OptArg
  transA (S.MOptArg ltxs) = mapM transL ltxs <&> S.MOptArg
  transA (S.SymArg ltx) = transL ltx <&> S.SymArg
  transA (S.MSymArg ltxs) = mapM transL ltxs <&> S.MSymArg
  transA (S.ParArg ltx) = transL ltx <&> S.ParArg
  transA (S.MParArg ltxs) = mapM transL ltxs <&> S.MParArg


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
        


countWords :: LaTeX  -> Int
countWords = getSum . fold (S.Sum . wordCount)

wordCount :: Text -> Int
wordCount = length . filter (all isUAlpha) . words . T.unpack where
  other :: [C.Char]
  other = "'·àèéíóòúïüç"
  isUAlpha c = C.isAlpha c || C.toLower c `elem` other 

chunks :: Int -> LaTeX  -> [Text]
chunks n = fold (\txt ->
  let nwords = length (T.words txt)
  in [txt | nwords > n])

firstWords :: LaTeX -> StateT Int IO LaTeX
firstWords = transform (\t -> do
    n <- get
    if n > 100
      then return ""
      else do
        put $ n + length (T.words t)
        return t)

split :: Int -> LaTeX -> (LaTeX, LaTeX)
split n = splitTo n where
  -- splitTo :: Int -> LaTeX -> (LaT)
  splitTo n lt@(S.TeXSeq lt1 lt2) =
    let 
      n1 = countWords lt1
      n2 = countWords lt2
    in  if n1 + n2 <= n
          then (lt, S.TeXEmpty)
          else if n1 <= n
            then
              let (lt3,lt4) = splitTo (n-n1) lt2
              in (lt1 <> lt3, lt4)
            else
              let (lt3,lt4) = splitTo n lt1
              in (lt3, lt4 <> lt2)
  splitTo _ lt = (lt, S.TeXEmpty) 
      


main :: IO ()
main = do
  let dir = "omega-target/"
  txt <- TIO.readFile $ dir ++ "30.tex"
  let Right latex = parseLaTeX txt
      pre = S.getPreamble latex
      Just body = S.getBody latex
      user = Just "jordi.saludes@upc.edu"
      mt = makeMT user "ca" "en"
  body' <- recoverTranslate mt body
  let latex' = pre <> document body'
  TIO.writeFile (dir ++ "30.ca.tex") $ render $ pre <> body'
      -- runStateT (firstWords latex) 0 <&> fst
      -- (lt1, lt2) = split 100 body
  --return (pre, body) -- <> lt1, pre <> lt2)