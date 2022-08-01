import LaTeXTranslation (translateLaTeXDoc, getDocStatus)
import Config (getConfig, useSync)
import XmlTranslation (xmlTranslate, decodeHTML, xmlMark)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as IOT
import System.Console.GetOpt
import Service
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv, getArgs)
import System.FilePath.Posix (splitExtension, (<.>))
import Data.Char (isSpace)

  
data Action = Remaining | Mark LangPair | Translate
data Options = Options 
  { optAction :: Action
  , optUser :: Maybe String
  , optFiles :: [FilePath]
  }

defaultOptions :: Options
defaultOptions = Options
  { optAction = Remaining
  , optUser = Nothing 
  , optFiles = []
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['u'] ["user"]
      (OptArg (\mu ops -> 
        case mu of
          Just u  -> return $ ops { optUser = Just u }
          Nothing -> do
            mu <- getUser
            putStrLn $ "\nUser is: " <> show mu
            return $ maybe ops (\u -> ops {optUser = Just u}) mu) "USER")
      "User for translation service. If not given, use MT_USER environment variable"

  , Option ['r'] ["remaining"]
      (NoArg (\ops -> return ops {optAction = Remaining}))
      "Show remaining segments"

  , Option ['t'] ["translate"]
      (NoArg (\ops -> return ops {optAction = Translate}))
      "Translate file"

  , Option ['m'] ["mark"]
      (ReqArg (\lp ops -> do
          let (src,':':dst) = break (==':') lp
          return ops {optAction = Mark (src,dst)})
        "SRC:DEST")
        "Mark file segments."
  ]

markXmlFile :: LangPair -> FilePath -> FilePath -> IO ()
markXmlFile lp inPath outPath = do
  contents <- readFile inPath
  let contents' = xmlMark lp contents
  writeFile outPath $ decodeHTML contents'
  putStrLn $ "Marked into " ++ outPath

  
translateLatexFile, translateXmlFile :: MTService -> FilePath -> FilePath -> IO ()
translateLatexFile mt in_path out_path = do
  contents <- IOT.readFile in_path
  cfg <- getConfig Nothing
  translation <- translateLaTeXDoc cfg mt contents
  IOT.writeFile out_path translation
  putStrLn $ "Translated into " ++ out_path

translateXmlFile mt in_path out_path = do
    cfg <- getConfig Nothing
    contents <- readFile in_path
    translation <- xmlTranslate (useSync cfg) mt contents
      -- let contents' = showTopElement root'
    writeFile out_path $ decodeHTML translation
    putStrLn $ "Translated into " ++ out_path
  

translateFile :: MTService -> FilePath -> IO ()
translateFile mt in_path = 
  case ext of
    ".ptx" -> translateXmlFile mt in_path out_path -- TODO: remove
    ".xml" -> translateXmlFile mt in_path out_path
    ".tex" -> translateLatexFile mt in_path out_path
  where
    (fname, ext) = splitExtension in_path
    out_path = fname <.> "trans" <.> ext

getUser :: IO (Maybe User)
getUser = lookupEnv "MT_USER"


computeRemaining :: FilePath -> IO ()
computeRemaining path = do
  cfg <- getConfig Nothing
  doc <- IOT.readFile path
  getDocStatus cfg doc

markFile :: LangPair -> FilePath -> IO ()
markFile lp in_path =
  case ext of 
      ".xml" -> markXmlFile lp in_path out_path
      ".ptx" -> markXmlFile lp in_path out_path -- TODO: remove
      e      -> error $ "No marking defined for " <> e
  where
    (fname, ext) = splitExtension in_path
    out_path = fname <.> "marked" <.> ext



main :: IO ()
main = do
  args  <- getArgs 
  let (actions, nonOptions, errors) =  getOpt RequireOrder options args
  if null errors
    then do
      opts <- foldl (>>=) (return defaultOptions) actions
      case optAction opts of
        Translate -> do
            muser <- maybe getUser (return . Just) (optUser opts)
            let mt =  makeMT muser
            -- let mt = dryRun -- TODO: for debugging the mt service
            putStrLn $ case user mt of
              Just u -> "user is: " ++ u
              _      -> "no user"
            mapM_ (translateFile mt) nonOptions
        Remaining -> mapM_ computeRemaining nonOptions
        Mark lp -> mapM_ (markFile lp) nonOptions
    else ioError (userError $ concat errors ++ usageInfo header options)
  where header = "Usage: ? [-uUSER] [-rt][-m SRC:DEST] files..."
  