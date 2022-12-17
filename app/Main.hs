import LaTeXTranslation (translateLaTeXDoc, getDocStatus, markLaTeX, markDocument)
import Config (getConfig, Config)
import XmlTranslation (xmlTranslate, decodeHTML)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as IOT
import System.Console.GetOpt
    ( getOpt,
      usageInfo,
      ArgDescr(ReqArg, OptArg, NoArg),
      ArgOrder(RequireOrder),
      OptDescr(..) )
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
  
      
translateLatexFile :: MTService -> FilePath -> FilePath -> IO ()
translateLatexFile mt in_path out_path = do
  contents <- IOT.readFile in_path
  cfg <- getConfig Nothing
  translation <- translateLaTeXDoc cfg mt contents
  IOT.writeFile out_path translation
  putStrLn $ "Translated into " ++ out_path


translateFile :: MTService -> FilePath -> IO ()
translateFile mt in_path = 
  case ext of
    -- ".xml" -> translateXmlFile mt in_path out_path
    ".tex" -> translateLatexFile mt in_path out_path
  where
    (fname, ext) = splitExtension in_path
    out_path = fname <.> "trans" <.> ext

getUser :: IO (Maybe User)
getUser = lookupEnv "MT_USER"


computeRemaining :: FilePath -> IO ()
markFile :: LangPair -> FilePath -> IO ()
computeRemaining path = do
  cfg <- getConfig Nothing
  doc <- IOT.readFile path
  getDocStatus cfg doc

markFile lp in_path =
  case ext of 
      -- ".xml" -> markXmlFile lp in_path out_path TODO: copy from ptx branch
      -- ".ptx" -> markXmlFile lp in_path out_path -- TODO: remove
      ".tex" -> markLaTeXFile lp in_path out_path
      e      -> error $ "No marking defined for " <> e
  where
    (fname, ext) = splitExtension in_path
    out_path = fname <.> "marked" <.> ext


markLaTeXFile :: LangPair -> FilePath -> FilePath -> IO ()
markLaTeXFile lp inPath outPath = do
  {-contents <- readFile inPath
  cfg <- getConfig Nothing
  contents' <- markDocument cfg lp contents
  -- let contents' = markLaTeX cfg lp contents
  writeFile outPath $ decodeHTML contents'
  putStrLn $ "Marked into " ++ outPath
-}
  contents <- IOT.readFile inPath
  cfg <- getConfig Nothing
  marked <- markDocument cfg lp contents
  IOT.writeFile outPath marked
  putStrLn $ "Marked into " ++ outPath




-- latexMark = markLaTeXFile
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
            putStrLn $ case user mt of
              Just u -> "user is: " ++ u
              _      -> "no user"
            mapM_ (translateFile mt) nonOptions
        Remaining -> mapM_ computeRemaining nonOptions
        Mark lp -> mapM_ (markFile lp) nonOptions
    else ioError (userError $ concat errors ++ usageInfo header options)
  where header = "Usage: ? [-uUSER] [-rt][-m SRC:DEST] files..."
  