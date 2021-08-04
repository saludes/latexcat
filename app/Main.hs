import Translation (latexTranslate)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as IOT
import System.Console.GetOpt
import Service
import System.Environment (lookupEnv, getArgs)
import Data.Char (isSpace)

  

data Options = Options
  { optLangs :: LangPair 
  , optSuffix :: String
  , optUser :: Maybe User
  , optFiles :: [FilePath ]
  }

defaultOptions :: Options
defaultOptions = Options
  { optLangs = ("es","ca")
  , optSuffix = ".out"
  , optUser = Nothing 
  , optFiles = []
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['f'] ["from-lang"]
      (ReqArg (\f ops -> return $ 
        let (_,t) = optLangs ops
        in ops { optLangs = (f,t)}) "LANG")
      "in ISO 2-char form" 
  , Option ['t'] ["to-lang"]
      (ReqArg (\t ops -> return $ 
        let (f,_) = optLangs ops
        in ops { optLangs = (f,t)}) "LANG")
      "in ISO 2-char form" 
  , Option ['o'] ["out-suffix"]
      (OptArg (\ms ops -> return $ maybe ops (\s -> ops { optSuffix = s}) ms) "EXT")
      "Suffix for output files"
  , Option ['u'] ["user"]
      (OptArg (\mu ops -> 
        case mu of
          Just u  -> return $ ops { optUser = Just u }
          Nothing -> do
            mu <- getUser
            putStrLn $ "\nUser is: " <> show mu
            return $ maybe ops (\u -> ops {optUser = Just u}) mu) "USER")
      "User for translation service"
  ]
      

translateFile :: MTService -> String -> FilePath -> IO ()
translateFile mt ext path = do
  contents <- IOT.readFile path
  translation <- latexTranslate mt contents
  let outPath = path ++ ext
  IOT.writeFile outPath translation
  putStrLn $ "Translated into " ++ outPath


getUser :: IO (Maybe User)
getUser = lookupEnv "MT_USER"

main :: IO ()
main = do
  args  <- getArgs 
  let (actions, nonOptions, errors) =  getOpt RequireOrder options args
  if null errors
    then do
      opts <- foldl (>>=) (return defaultOptions) actions
      let (lin,lout) = optLangs opts
          mt = makeMT (optUser opts)  lin lout
          ext = optSuffix opts
      mapM_ (translateFile mt ext) nonOptions
    else ioError (userError $ concat errors ++ usageInfo header options)
  where header = "Usage: ? -f LANG -t LANG [OPTION..] files..."
  