{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ConfigFile where

import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import System.Directory
import GHC.Generics
import System.FilePath (pathSeparator)
import Data.Aeson

configAppName :: String
configAppName = "ttc-predictions"

configFileName :: String
configFileName = "ttc-predictions-config.json"

getPreferredConfigFilePath :: IO String
getPreferredConfigFilePath = do
  xdgExists <- getXdgDirectory XdgConfig "" >>= doesPathExist
  if xdgExists
    then do
          getXdgDirectory XdgConfig configAppName >>= return . (++configFileName) . (++[pathSeparator])
    else
        getAppUserDataDirectory configAppName >>= return . (++configFileName) . (++[pathSeparator])


data ConfigFile = ConfigFile {
    cf_agency :: Text
  , cf_route :: Text
  , cf_stop :: Text
  , cf_preferred_output_format :: Maybe Text
  } deriving (Eq, Show, Generic)

cfOptions :: Options
cfOptions = defaultOptions {
  fieldLabelModifier = drop 3
  }

instance FromJSON ConfigFile where
  parseJSON = genericParseJSON cfOptions

instance ToJSON ConfigFile where
  toEncoding = genericToEncoding cfOptions


getConfigFile :: IO (Maybe ConfigFile)
getConfigFile = do
  fpath <- getPreferredConfigFilePath
  exists <- doesFileExist fpath
  if exists
    then BL.readFile fpath >>=
          return . decode
    else return Nothing
