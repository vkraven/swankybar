{-# LANGUAGE OverloadedStrings #-}

module DbusHelper where

import DBus.Notify
import DBus.Client (ClientError)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Control.Monad.Reader
import Control.Exception

data NotifyMessage = NotifyMessage {
    toggleSuccess :: Bool
  , notifyClient :: Client
  , focusedOutputName :: Maybe Text
  , previousSyncStatus :: Bool
  }

thisAppName :: String
thisAppName = "swankybar - Adaptive Sync"

errorNote :: Note
errorNote = blankNote {
    appName = thisAppName
  , summary = "Error"
  , body = Just $ Bold (Text "ERROR: Toggling adaptive sync failed")
  }

-- makeToggleStatusNote focusedNameOrAll previousStatusWasActive
makeToggleStatusNote :: Maybe Text -> Bool -> Note
makeToggleStatusNote Nothing True = blankNote {
    appName = thisAppName
  , summary = "Disabled"
  , body = Just $ Text "Successfully disabled adaptive sync on all outputs"
  }
makeToggleStatusNote Nothing False = blankNote {
    appName = thisAppName
  , summary = "Enabled"
  , body = Just $ Text "Successfully enabled adaptive sync on all outputs"
  }
makeToggleStatusNote (Just outputName) True = blankNote {
    appName = thisAppName
  , summary = "Disabled"
  , body = Just $ Text $ "Successfully disabled adaptive sync on output: " ++ T.unpack outputName
  }
makeToggleStatusNote (Just outputName) False = blankNote {
    appName = thisAppName
  , summary = "Enabled"
  , body = Just $ Text $ "Successfully enabled adaptive sync on output: " ++ T.unpack outputName
  }

notifyToggleError :: ReaderT NotifyMessage IO ()
notifyToggleError = do
  client <- asks notifyClient
  liftIO $ notify client errorNote
  return ()

notifyToggleSuccess :: ReaderT NotifyMessage IO ()
notifyToggleSuccess = do
  client <- asks notifyClient
  foName <- asks focusedOutputName
  prev <- asks previousSyncStatus
  liftIO $ notify client $ makeToggleStatusNote foName prev
  return ()

connectAndNotify :: Bool -> Maybe Text -> Bool -> IO ()
connectAndNotify False _ _ = do
  client <- connectSession
  let env = NotifyMessage False client Nothing False
  runReaderT notifyToggleError env
connectAndNotify True foName prevStatus = do
  client <- connectSession
  let env = NotifyMessage True client foName prevStatus
  runReaderT notifyToggleSuccess env


-- If we cannot connect to the dbus session, just ignore it
-- DBus notifications are not required
safeConnectAndNotify :: Bool -> Maybe Text -> Bool -> IO ()
safeConnectAndNotify a b c = catch (connectAndNotify a b c) ((\e -> return ()) :: ClientError -> IO ())

