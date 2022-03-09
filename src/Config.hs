{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Config where
import Data.Text (Text)
import qualified Data.Yaml as Y
import Text.RawString.QQ
import Data.Yaml (FromJSON(..), (.:))
import Data.ByteString (ByteString)
import qualified System.Directory as Dir

configYml :: ByteString
configYml = [r|
environments:
  - equation
  - align
  # is this standard?
  - wrapfigure
  - figure
commands:
  # Style
  - emph
  - em 
  - it
  # Sectioning
  - title
  - chapter
  - section
  - subsection
  - subsubsection
  # Other
  - footnote
  - caption
|]

data Config = 
    Cfg {
      environments :: [String]
    , commands :: [String]
    } deriving (Eq, Show)

instance FromJSON Config where
    parseJSON (Y.Object v) = 
        Cfg <$>
        v .: "environments" <*>
        v .: "commands"
    parseJSON _ = fail "Expected Object for Config value"


configFile :: FilePath 
configFile = "config"

getConfig :: Maybe FilePath -> IO Config
getConfig (Just path) = getConfigFromFile path
getConfig _  = do 
    exists <- Dir.doesFileExist configFile
    if exists 
        then getConfigFromFile configFile
        else Y.decodeThrow configYml
getConfigFromFile path = do 
    putStrLn $ "Using config from: " ++ path
    Y.decodeFileThrow path