module Translation where
import Service
import Text.LaTeX.Base.Parser
import Text.LaTeX
import qualified Text.LaTeX.Base.Syntax as S
import Data.Text
import Data.Functor ((<&>))



latexTranslate :: MTService -> Text -> IO Text
latexTranslate mt src_text = 
    case parseLaTeX src_text of
        Left err -> fail $ show err
        Right latex  -> do
            latex' <- translate mt latex
            return  $ render latex'


translate :: MTService -> LaTeX -> IO LaTeX
translate service l = tr l 
  where
    q = query service
    tr l = case l of
      S.TeXRaw text       -> q text <&> S.TeXRaw
      S.TeXComment text   -> pure $ S.TeXComment text
      S.TeXSeq l1 l2      -> do
          d1 <- tr l1
          d2 <- tr l2
          pure $ S.TeXSeq d1 d2
      S.TeXEnv com args l -> tr l <&> S.TeXEnv com args
      S.TeXBraces l       -> tr l <&> S.TeXBraces
      l                 -> pure l
    





