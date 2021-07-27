import Test.Hspec
import Service
import Translation
import System.FilePath.Posix
import qualified Data.Text.IO as IOT
import Data.Text
import Control.Monad.IO.Class
-- import Data.Algorithm.Diff

main :: IO ()
main = hspec do
    describe "LaTeX trans" do
        it "must translate the samples" do
            let Just pair = makeLangPair "en" "es"
                get = getSampleFor pair
                lines = Data.Text.lines
            (trans_text,dst_text) <-
                liftIO do
                    Sample [(l1,src), (l2,dst)] <- get 1  
                    let mt = makeMT Nothing l1 l2
                    dst' <- latexTranslate mt src
                    return (lines dst', lines dst)
            trans_text `shouldBe` dst_text

newtype Sample = Sample [(ISOLang, Text)]


getSampleFor :: LangPair -> Int -> IO Sample
getSampleFor pair@(l1,l2) n = do
        text1 <- samp l1
        text2 <- samp l2
        return $ Sample [(l1,text1), (l2,text2)]
    where
        samp l = IOT.readFile $ samples </> show n <.> l
        samples = "test/samples/"
        


getLang :: FilePath -> ISOLang
getLang = takeExtension
