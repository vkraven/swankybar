{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Outputs
import Data.Aeson (encode)
import Control.Monad.Trans.Maybe
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified CmdArgs as ARGS
import AdaptiveSync
import WaybarIO (reloadAllWaybarConfig)

displayError :: IO ()
displayError = TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $
                 WaybarJSON (getSyncStatusIcon ASError)
                            (show ASError)
                            (makeDetailedAdaptiveSyncMessage ASError [])

displayWaybar :: IO ()
displayWaybar = do
  maybeOuts <- runMaybeT getOutputs
  case maybeOuts of
    Nothing -> displayError
    Just outs -> do
      status <- getSingleAdaptiveSyncStatus
      TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $
        WaybarJSON (getSyncStatusIcon status)
                   (show status)
                   (makeDetailedAdaptiveSyncMessage status outs)

-- Waybar gets quite confused when Sway switches AS modes
-- while it is trying to update its own bar.
-- Adding a user-signalled-Waybar-reload instruction to address this.
toggleAndDisplayWaybar :: Bool -> IO ()
toggleAndDisplayWaybar False = do
  toggleResult <- toggleFocusedOutputAdaptiveSync
  if toggleResult
    then displayWaybar >> reloadAllWaybarConfig
    else displayError
toggleAndDisplayWaybar True = do
  toggleResult <- toggleAllOutputsAdaptiveSync
  if toggleResult
    then displayWaybar >> reloadAllWaybarConfig
    else displayError

handleArgs :: ARGS.WaybarAdaptive -> IO ()
handleArgs ARGS.Display = displayWaybar
handleArgs (ARGS.Toggle all_) = toggleAndDisplayWaybar all_

main :: IO ()
main = do
  ARGS.runSwankybarAdaptiveCli >>= handleArgs
