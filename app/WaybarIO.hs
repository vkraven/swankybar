{-# LANGUAGE OverloadedStrings #-}

module WaybarIO where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process
import Control.Exception
import Text.Read (readMaybe)
import System.Posix.Types (CPid)
import Data.Maybe (isJust, fromMaybe)
import GHC.IO.Exception (IOException(IOError))
import System.Posix.Signals (userDefinedSignal2, signalProcess)


runPs :: IO Text
runPs = readProcess "ps" ["-a", "-o", "pid", "-o", "comm"] "" >>= return . T.pack

findWaybarLines :: Text -> [Text]
findWaybarLines =
  filter (T.isInfixOf "waybar") . T.lines . T.toLower

findWaybarPIDs :: Text -> [CPid]
findWaybarPIDs =
  fromMaybe [] . sequence . filter isJust . fmap getFirstIfNotEmpty . fmap T.words . findWaybarLines
    where getFirstIfNotEmpty [] = Nothing
          getFirstIfNotEmpty (x:_) = readMaybe . T.unpack $ x

psFindWaybarPids :: IO [CPid]
psFindWaybarPids = runPs >>= return . findWaybarPIDs

-- Once again, these nice-to-haves should not crash the application
-- So let it return nothing and continue
safePsFindWaybarPids :: IO [CPid]
safePsFindWaybarPids = catch psFindWaybarPids ((\e -> return []) :: IOError -> IO [CPid])


-- Sending SIGUSR2 to Waybar > v0.9.5 reloads the config
-- see https://github.com/Alexays/Waybar/wiki/FAQ#how-can-i-reload-the-configuration-without-restarting-waybar
reloadWaybarConfig :: [CPid] -> IO ()
reloadWaybarConfig = mapM_ (signalProcess userDefinedSignal2)

reloadAllWaybarConfig :: IO ()
reloadAllWaybarConfig = safePsFindWaybarPids >>= reloadWaybarConfig
