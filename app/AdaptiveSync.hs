{-# LANGUAGE OverloadedStrings #-}

module AdaptiveSync where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Maybe
import Data.Aeson (decode)
import Data.List (intersperse)

import Outputs
import SwayIPC
import DbusHelper


data AdaptiveSyncStatus = ASActive
                        | ASInactive
                        | ASPartial
                        | ASError
                        deriving (Eq, Ord)

instance Show AdaptiveSyncStatus where
  show ASActive = "Active"
  show ASInactive = "Inactive"
  show ASPartial = "Partial"
  show ASError = "ERROR"

interpretAdaptiveSync :: Text -> Bool
interpretAdaptiveSync "enabled" = True
interpretAdaptiveSync _ = False


ipcCommandWasSuccessful :: Maybe ByteString -> Bool
ipcCommandWasSuccessful Nothing = False
ipcCommandWasSuccessful (Just msg) =
  case (decode . BL.fromStrict $ msg :: Maybe [M.Map String Bool]) of
    Nothing -> False
    Just listMap ->
      case listMap of
        [] -> False
        [x] -> fromMaybe False $ M.lookup "success" x
        x@(_:_) -> all id $ fromMaybe False . M.lookup "success" <$> x

getOutputs :: MaybeT IO [Output]
getOutputs = MaybeT ipcGetOutputs >>=
              MaybeT . return . decode . BL.fromStrict

getFocusedOutputName :: [Output] -> Maybe Text
getFocusedOutputName outs =
  case filter o_focused outs of
    [] -> Nothing
    [x] -> Just $ o_name x
    (x:_) -> Nothing

ipcGetFocusedOutputName :: MaybeT IO Text
ipcGetFocusedOutputName =
  getOutputs >>= MaybeT . return . getFocusedOutputName

adaptiveSyncStatusByName :: [Output] -> Text -> Maybe Bool
adaptiveSyncStatusByName outs name =
  case filter (\x -> o_name x == name) outs of
    [] -> Nothing
    [x] -> Just . interpretAdaptiveSync . o_adaptive_sync_status $ x
    _ -> Nothing

toggleFocusedOutputAdaptiveSync :: IO Bool
toggleFocusedOutputAdaptiveSync = do
  maybeOuts <- runMaybeT getOutputs
  case maybeOuts of
    Nothing -> return False
    Just outs ->
      case getFocusedOutputName outs of
        Nothing -> return False
        Just focusedOutput ->
          case adaptiveSyncStatusByName outs focusedOutput of
            Nothing -> return False
            Just currentStatus ->
              let commandMessage = BS.concat [
                      "output "
                    , TE.encodeUtf8 focusedOutput
                    , " adaptive_sync "
                    , syncStatusToCommand (not currentStatus)
                    ]
               in ipcSendCommand commandMessage >>=
                   return . ipcCommandWasSuccessful >>=
                    \success -> safeConnectAndNotify success (Just focusedOutput) currentStatus >>
                     return success

toggleAllOutputsAdaptiveSync :: IO Bool
toggleAllOutputsAdaptiveSync = do
  maybeOuts <- runMaybeT getOutputs
  syncStatus <- getSingleAdaptiveSyncStatus
  case maybeOuts of
    Nothing -> return False
    Just outs ->
      case syncStatus of
        ASError -> return False
        _       ->
          let currentStatus = not (syncStatus == ASInactive)
              commandMessage = BS.concat [
                  "output * adaptive_sync "
                , syncStatusToCommand (not currentStatus)
                ]
           in ipcSendCommand commandMessage >>=
               return . ipcCommandWasSuccessful >>=
                \success -> safeConnectAndNotify success Nothing currentStatus >>
                 return success



syncStatusToCommand :: Bool -> ByteString
syncStatusToCommand True = "on"
syncStatusToCommand False = "off"

getSyncStatusIcon :: AdaptiveSyncStatus -> String
getSyncStatusIcon ASActive = "\xf04e6"
getSyncStatusIcon ASPartial = "\xf11a2"
getSyncStatusIcon ASInactive = "\xf04e8"
getSyncStatusIcon ASError = "\xf04e7"

makeDetailedOutputMessage :: Output -> String
makeDetailedOutputMessage out = "\xf0379" ++ " " ++
                                 (T.unpack . o_name $ out) ++
                                 ": " ++ (T.unpack . o_model $ out ) ++ " - " ++
                                 (T.unpack . o_adaptive_sync_status $ out)

makeDetailedAdaptiveSyncMessage :: AdaptiveSyncStatus -> [Output] -> String
makeDetailedAdaptiveSyncMessage ASError _ = "<big>ERROR</big>"
makeDetailedAdaptiveSyncMessage status outs = "<big>" ++ show status ++ "</big>\n\n" ++
                                                (concat . intersperse "\n" . fmap makeDetailedOutputMessage $ outs)

getAllAdaptiveSyncStatus :: MaybeT IO (Bool, Bool)
getAllAdaptiveSyncStatus = do
  outs <- getOutputs
  let allAS = all (interpretAdaptiveSync . o_adaptive_sync_status) outs
      anyAS = any (interpretAdaptiveSync . o_adaptive_sync_status) outs
  return (allAS, anyAS)

multipleASToSingle :: (Bool, Bool) -> AdaptiveSyncStatus
multipleASToSingle (True, True) = ASActive
multipleASToSingle (False, True) = ASPartial
multipleASToSingle (False, False) = ASInactive
multipleASToSingle (True, False) = ASError

getSingleAdaptiveSyncStatus :: IO AdaptiveSyncStatus
getSingleAdaptiveSyncStatus = do
  allASStatus <- runMaybeT getAllAdaptiveSyncStatus
  case allASStatus of
    Nothing -> return ASError
    Just x -> return $ multipleASToSingle x
