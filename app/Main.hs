import LaTeXTranslation (latexTranslate)
import XmlTranslation (xmlTranslate, decodeHTML)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as IOT
import System.Console.GetOpt
import Service
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv, getArgs)
import System.FilePath.Posix (splitExtension, (<.>))
import Data.Char (isSpace)

  

data Options = Options
  { optLangs  :: LangPair 
  , optUser   :: Maybe User
  , optFiles  :: [FilePath ]
  }

defaultOptions :: Options
defaultOptions = Options
  { optLangs = ("","")
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
  , Option ['u'] ["user"]
      (OptArg (\mu ops -> 
        case mu of
          Just u  -> return $ ops { optUser = Just u }
          Nothing -> do
            mu <- getUser
            putStrLn $ "\nUser is: " <> show mu
            return $ maybe ops (\u -> ops {optUser = Just u}) mu) "USER")
      "User for translation service. If not given, use MT_USER environment variable"
  ]
      
translateLatexFile, translateXmlFile :: MTService -> FilePath -> FilePath -> IO ()
translateLatexFile mt in_path out_path = do
  contents <- IOT.readFile in_path
  translation <- latexTranslate mt contents
  IOT.writeFile out_path translation
  putStrLn $ "Translated into " ++ out_path

translateXmlFile mt in_path out_path = do
    contents <- readFile in_path
    translation <- xmlTranslate mt contents
      -- let contents' = showTopElement root'
    writeFile out_path $ decodeHTML translation
    putStrLn $ "Translated into " ++ out_path
  

translateFile :: MTService -> FilePath -> IO ()
translateFile mt in_path = 
  case ext of
    ".xml" -> translateXmlFile mt in_path out_path
    ".tex" -> translateLatexFile mt in_path out_path
  where
    (fname, ext) = splitExtension in_path
    lang = snd $ pair mt
    out_path = fname <.> lang <.> ext

getUser :: IO (Maybe User)
getUser = lookupEnv "MT_USER"

main :: IO ()
main = do
  args  <- getArgs 
  let (actions, nonOptions, errors) =  getOpt RequireOrder options args
  if null errors
    then do
      opts <- foldl (>>=) (return defaultOptions) actions
      case optLangs opts of
        ("",_)     -> ioError (userError "Must specify 'from' LANG")
        (_,"")     -> ioError (userError "Must specify 'to LANG")
        (lin,lout) -> do
            let mt = makeMT (optUser opts)  lin lout
            mapM_ (translateFile mt) nonOptions
    else ioError (userError $ concat errors ++ usageInfo header options)
  where header = "Usage: ? -f LANG -t LANG [OPTION..] files..."
  