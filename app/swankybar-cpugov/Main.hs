{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Outputs
import Data.Aeson (encode)
import Control.Monad.Trans.Maybe
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified CmdArgs as ARGS
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (sortOn, intersperse)
import Control.Exception (catch, IOException)
import System.Directory
import qualified Data.Set as S

data CpuInfo = CpuInfo {
    ci_name :: String
  , ci_driver :: String
  , ci_governor :: String
  } deriving (Eq)

data ScalingInput = Governor | Driver deriving (Eq, Show)

instance Show CpuInfo where
  show x = ci_name x ++ ": " ++
           ci_driver x ++ " - " ++
           ci_governor x

sysfsPrefixPath :: String
sysfsPrefixPath = "/sys/devices/system/cpu/"

cpuFreq :: String
cpuFreq = "cpufreq"

fileIsReadable :: String -> IO Bool
fileIsReadable path =
  (getPermissions path >>= return . readable)
    `catch` ((\e -> return False) :: IOException -> IO Bool)

getReadableScalingInput :: String -> ScalingInput -> MaybeT IO String
getReadableScalingInput cpuName Governor =
  MaybeT $ findFileWith fileIsReadable [sysfsPrefixPath ++ cpuName ++ "/" ++ cpuFreq] "scaling_governor"
getReadableScalingInput cpuName Driver =
  MaybeT $ findFileWith fileIsReadable [sysfsPrefixPath ++ cpuName ++ "/" ++ cpuFreq] "scaling_driver"

cleanSysFsVal :: String -> String
cleanSysFsVal = T.unpack . T.strip . T.pack

retrieveCpuInfoByName :: String -> MaybeT IO CpuInfo
retrieveCpuInfoByName cpuName = do
  sDriver <- getReadableScalingInput cpuName Driver
  sGovernor <- getReadableScalingInput cpuName Governor
  sDriverVal <- MaybeT $ readFile sDriver >>= return . Just . cleanSysFsVal
  sGovernorVal <- MaybeT $ readFile sGovernor >>= return . Just . cleanSysFsVal
  return $ CpuInfo cpuName sDriverVal sGovernorVal

sysfsCpuFormatInteger :: String -> Maybe Integer
sysfsCpuFormatInteger path =
  T.stripPrefix "cpu" (T.pack path) >>=
   readMaybe . T.unpack

followsSysfsCpuFormat :: String -> Bool
followsSysfsCpuFormat = isJust . sysfsCpuFormatInteger

getCpuNames :: IO [String]
getCpuNames = do
  dirExists <- doesDirectoryExist sysfsPrefixPath
  if dirExists
    then listDirectory sysfsPrefixPath >>=
          return . sortOn sysfsCpuFormatInteger . filter followsSysfsCpuFormat
    else return []

getAllCpuInfo :: IO [CpuInfo]
getAllCpuInfo =
  getCpuNames >>=
   return . fmap retrieveCpuInfoByName >>=
    traverse runMaybeT >>=
     return . fromMaybe [] . sequence .  filter isJust

allCpuInfoToStatus :: [CpuInfo] -> String
allCpuInfoToStatus [] = "Error"
allCpuInfoToStatus x@(_:_) =
  let govs = ci_governor <$> x
  in
    case S.size . S.fromList $ govs of
      0 -> "Error"
      1 -> head govs
      _ -> "Mixed"

displayCpuGovWaybar :: IO ()
displayCpuGovWaybar = do
  cpus <- getAllCpuInfo
  TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $
    WaybarJSON (allCpuInfoToStatus cpus)
               (allCpuInfoToStatus cpus)
               (concat . intersperse "\n" . fmap show $ cpus)

handleArgs :: ARGS.SwankyCpuGov -> IO ()
handleArgs ARGS.Display = displayCpuGovWaybar

main :: IO ()
main = do
  ARGS.runSwankybarCpuGovCli >>= handleArgs
