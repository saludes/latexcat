module LaTeXTranslation where
import Service
import Text.LaTeX.Base.Parser
import Text.LaTeX hiding (words)
import qualified Text.LaTeX.Base.Syntax as S
import Data.Text -- hiding (map)
import Data.Functor ((<&>))



latexTranslate :: MTService -> Text -> IO Text
latexTranslate mt src_text = 
    case parseLaTeX src_text of
        Left err -> fail $ show err
        Right latex  -> do
            latex' <- translate mt latex
            return  $ render latex'



translate :: MTService -> LaTeX -> IO LaTeX
translate service l = tr False l 
  where
    q = query service
    tr :: Bool -> LaTeX -> IO LaTeX
    tr True l   = pure l
    tr inmath l = case l of
      S.TeXRaw text        -> q text <&> S.TeXRaw
      S.TeXComment text    -> pure $ S.TeXComment text
      -- S.TeXCommS comm     -> pure l
      S.TeXComm comm args  -> if mustTranslateCommArgs comm
                                then mapM (traverseArgs (tr inmath)) args  <&> S.TeXComm comm
                                else pure l
      S.TeXSeq l1 l2       -> do
          d1 <- tr inmath l1
          d2 <- tr inmath l2
          pure $ S.TeXSeq d1 d2
      S.TeXEnv env args l' -> if mustTranslateEnvContent env
                              then tr False l' <&> S.TeXEnv env args
                              else pure l
      S.TeXBraces l        -> tr inmath l <&> S.TeXBraces
      l                    -> pure l


traverseArgs :: (LaTeX -> IO LaTeX) -> S.TeXArg -> IO S.TeXArg 
traverseArgs f (S.FixArg l)   = f l <&> S.FixArg
traverseArgs f (S.OptArg l)   = f l <&> S.OptArg 
traverseArgs f (S.MOptArg ls) = mapM f ls <&> S.MOptArg 
traverseArgs f (S.SymArg l)   = f l <&> S.SymArg
traverseArgs f (S.MSymArg ls) = mapM f ls <&> S.MSymArg
traverseArgs f (S.ParArg l)   = f l <&> S.ParArg
traverseArgs f (S.MParArg ls) = mapM f ls <&> S.MParArg
    

mustTranslateCommArgs, mustTranslateEnvContent :: String -> Bool 
mustTranslateEnvContent = not . (`elem` mathEnvs)
  where mathEnvs = Prelude.words "equation align"
mustTranslateCommArgs = (`elem` transComms)
  where transComms = Prelude.words "emph it"




