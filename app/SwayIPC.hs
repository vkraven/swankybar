{-# LANGUAGE OverloadedStrings #-}

module SwayIPC where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as Bin
import Data.Int (Int32)
import Control.Monad.Reader
import Network.Socket
import Network.Socket.ByteString
import System.Environment (lookupEnv)

swayIpcMagic :: ByteString
swayIpcMagic = "i3-ipc"

data SwayIPCInstruction = SwayCommand
                        | SwayGetOutputs
                        deriving (Eq, Ord, Show)

instructionToInteger :: SwayIPCInstruction -> Integer
instructionToInteger SwayCommand = 0
instructionToInteger SwayGetOutputs = 3

makeSwayMessage :: SwayIPCInstruction -> ByteString -> (ByteString, ByteString)
makeSwayMessage inst payload =
  (BS.concat [
      swayIpcMagic
    , integerToBin . fromIntegral $ BS.length payload
    , integerToBin $ instructionToInteger inst
    ],
    payload)

integerToBin :: Integer -> ByteString
integerToBin i =
  BS.reverse . BL.toStrict . Bin.encode $ finalInt
    where finalInt = fromIntegral i :: Int32

integerFromBin :: ByteString -> Integer
integerFromBin bs =
  fromIntegral ((Bin.decode . BL.fromStrict . BS.reverse $ bs) :: Int32)

dropIpcMagic :: ByteString -> Maybe ByteString
dropIpcMagic bs =
  if BS.take (BS.length swayIpcMagic) bs == swayIpcMagic
    then Just $ BS.drop (BS.length swayIpcMagic) bs
    else Nothing

getIpcResponseLength :: ByteString -> Maybe Integer
getIpcResponseLength bs =
  dropIpcMagic bs >>= return . integerFromBin . BS.take 4

localSock :: Reader String SockAddr
localSock = ask >>= return . SockAddrUnix

ipcGetOutputs :: IO (Maybe ByteString)
ipcGetOutputs =
  let (msg, payload) = makeSwayMessage SwayGetOutputs ""
   in do
       maybeSwaySock <- lookupEnv "SWAYSOCK"
       case maybeSwaySock of
         Nothing -> return Nothing
         Just swaySock ->
           let lSock = runReader localSock swaySock
            in do
                soc <- socket AF_UNIX Stream 0
                connect soc lSock
                send soc msg
                send soc payload
                res1 <- recv soc (BS.length swayIpcMagic + 8)
                case getIpcResponseLength res1 of
                  Nothing -> close soc >> return Nothing
                  Just resLen -> do
                    res2 <- recv soc $ fromIntegral resLen
                    close soc
                    return $ Just res2

ipcSendCommand :: ByteString -> IO (Maybe ByteString)
ipcSendCommand cmd =
  let (msg, payload) = makeSwayMessage SwayCommand cmd
   in do
    maybeSwaySock <- lookupEnv "SWAYSOCK"
    case maybeSwaySock of
      Nothing -> return Nothing
      Just swaySock -> do
        soc <- socket AF_UNIX Stream 0
        connect soc $ runReader localSock swaySock
        send soc msg
        send soc payload
        res1 <- recv soc (BS.length swayIpcMagic + 8)
        case getIpcResponseLength res1 of
          Nothing -> close soc >> return Nothing
          Just resLen -> do
            res2 <- recv soc $ fromIntegral resLen
            close soc
            return $ Just res2
