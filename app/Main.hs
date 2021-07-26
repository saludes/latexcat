import Translation (latexTranslate)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Service
import System.Environment (lookupEnv)

--import Debug.Trace
import Data.Char (isSpace)



getUser :: IO (Maybe User)
getUser = lookupEnv "MT_USER"


main :: IO ()
main = do
  muser <- getUser
  putStrLn $ "\nUser is: " <> show muser
  let mt = makeMT muser "en" "es" 
  trans <- latexTranslate mt example
  putStrLn ""
  T.putStr  trans

reveal :: T.Text -> T.Text
reveal "" = "=\n"
reveal t = T.map (\c -> if isSpace c then '*' else c) t

example :: T.Text
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
  , "displayed math $$\\ln 2 = \\int_1^2\\frac{dx}{x}.$$"
  , "\\end{document}"
    ]
