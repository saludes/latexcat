import Text.LaTeX
import qualified Text.LaTeX.Base.Syntax as S
import Text.LaTeX.Base.Parser
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Service as MT
import Data.Functor ((<&>))

--import Debug.Trace
import Data.Char (isSpace)


count :: String -> Text -> Text
count suffix text = T.pack $ "<" ++ show nwords ++ " words " ++ suffix ++ ">\n" 
  where nwords = length $ T.words text


translate :: MT.Config -> LaTeX -> IO LaTeX
translate config l = tr l 
  where
    tr l = case l of
      S.TeXRaw text     -> MT.translate config text <&> S.TeXRaw
      S.TeXComment text   -> pure $ S.TeXComment text
      S.TeXSeq l1 l2      -> do
          d1 <- tr l1
          d2 <- tr l2
          pure $ S.TeXSeq d1 d2
      S.TeXEnv com args l -> tr l <&> S.TeXEnv com args
      S.TeXBraces l       -> tr l <&> S.TeXBraces
      l                 -> pure l
    
    
main :: IO ()
main = case parseLaTeX example of
  Left err -> print err
  Right l  -> do
    putStrLn "Printing LaTeX AST..."
    print l
    let cfg = MT.makeConfig "en" "es" 
    lt <- translate cfg l
    let t = render lt
    putStrLn "\n"
    --for_ (T.lines t) (T.putStr . reveal)
    T.putStr  t

reveal :: Text -> Text
reveal "" = "=\n"
reveal t = T.map (\c -> if isSpace c then '*' else c) t

example :: Text
example = T.unlines
  [ "\\documentclass{article}"
  , "\\usepackage[utf8]{inputenc}"
  , "\\author{Daniel Díaz}"
  , "\\title{LaTeX parser}"
  , "\\begin{document}"
  , "\\maketitle"
  , "% Here comes the document"
  , "This is an example of how to parse LaTeX using the"
  , "\\HaTeX library with inline $x+3$ and"
  , "displayed math $$\\int_1^1\\frac{dx}{x}.$$"
  , "\\end{document}"
    ]
