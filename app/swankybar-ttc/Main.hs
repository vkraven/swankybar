{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Outputs
import Data.Aeson (encode)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified CmdArgs as ARGS
import qualified Data.Text as T
import Data.List (sortOn, intersperse)
import qualified Data.Set as S
import Data.Text (Text)

import System.IO (hPutStrLn, stderr)

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import TTCXml
import Printer
import ConfigFile


ttcEndpoint :: String
ttcEndpoint = "https://webservices.umoiq.com/service/publicXMLFeed"

data TTCCommand = RouteList { rl_agency :: String }
                | Predictions { p_agency :: String
                              , p_stop :: String
                              , p_route :: String }
                | RouteConfig { rc_agency :: String
                              , rc_route :: String }
                deriving (Eq, Ord, Show)

makeTTCCommand :: TTCCommand -> String
makeTTCCommand (RouteList a) = ttcEndpoint ++ "?command=routeList&a=" ++ a
makeTTCCommand (Predictions a s r) = ttcEndpoint ++ "?command=predictions" ++
                                      "&a=" ++ a ++
                                      "&s=" ++ s ++
                                      "&r=" ++ r
makeTTCCommand (RouteConfig a r) = ttcEndpoint ++ "?command=routeConfig" ++
                                    "&a=" ++ a ++
                                    "&r=" ++ r

makeTTCRequest :: TTCCommand -> IO Text
makeTTCRequest cmd =
  let endpoint = makeTTCCommand cmd
   in do
    man <- newTlsManager
    req <- parseRequest endpoint
    res <- httpLbs req man
    return . TE.decodeUtf8 . BL.toStrict . responseBody $ res

getRouteList :: String -> IO (Maybe [TTCRoute])
getRouteList agency =
  makeTTCRequest (RouteList agency) >>=
    return . xmlToTTCRoutes

getDirectionStopsByRoute :: String -> String -> IO (Maybe [TTCDirection])
getDirectionStopsByRoute agency rtTag =
  makeTTCRequest (RouteConfig agency rtTag) >>=
    return . xmlToTTCDirections

getPredictionsByRouteAndStop :: String -> String -> String -> IO (Maybe [TTCPrediction])
getPredictionsByRouteAndStop agency rtTag stpTag =
  makeTTCRequest (Predictions agency stpTag rtTag) >>=
    return . xmlToTTCPredictions

writeToStdErr :: String -> IO ()
writeToStdErr = hPutStrLn stderr

runSearchCommand :: String -> Maybe String -> Maybe String -> ARGS.OutputFormat -> IO ()
runSearchCommand _ _ _ ARGS.Waybar = writeToStdErr "waybar is not a supported search output format"
runSearchCommand _ _ _ ARGS.Polybar = writeToStdErr "polybar is not a supported search output format"
runSearchCommand agency Nothing Nothing outformat = do
  routes <- getRouteList agency
  case routes of
    Nothing -> TIO.putStrLn $ T.concat ["No routes found for agency: ", T.pack agency]
    Just rts -> case outformat of
                   ARGS.Pretty -> TIO.putStrLn $ prettyPrintRouteList rts
                   ARGS.Plain -> TIO.putStrLn $ plainPrintRouteList rts
runSearchCommand agency (Just route) Nothing outformat = do
  dirs <- getDirectionStopsByRoute agency route
  case dirs of
    Nothing -> TIO.putStrLn $ T.concat ["No results found for agency: ", T.pack agency, " and route: ", T.pack route]
    Just d -> case outformat of
                   ARGS.Pretty -> TIO.putStrLn $ prettyPrintDirections d
                   ARGS.Plain -> TIO.putStrLn $ plainPrintDirections d
runSearchCommand agency (Just route) (Just stop) _ = do
  validated <- validateRouteAndStop agency route stop
  case validated of
    True -> TIO.putStrLn  (T.concat ["\nPlease run: "
                                    , "swankybar-ttc -a "
                                    , T.pack agency
                                    , " -r "
                                    , T.pack route
                                    , " -s "
                                    , T.pack stop
                                    , "\n\nTo save this permanently, please run: swankybar-ttc config --save -a "
                                    , T.pack agency
                                    , " -r "
                                    , T.pack route
                                    , " -s "
                                    , T.pack stop])
    False -> TIO.putStrLn "False"
runSearchCommand _ _ _ _ = TIO.putStrLn "Not implemented yet"

validateRouteAndStop :: String -> String -> String -> IO Bool
validateRouteAndStop agency route stop = do
  dirs <- getDirectionStopsByRoute agency route
  case dirs of
    Nothing -> TIO.putStrLn (T.concat ["INVALID: No results found for agency: ", T.pack agency, " and route: ", T.pack route])
                  >> return False
    Just d ->
      case filter (\x -> (T.pack stop) `elem` (stopTag <$> directionStops x)) d of
        [] ->
          TIO.putStrLn  (T.concat ["INVALID: Stop ID: "
                                  , T.pack stop
                                  , " is not serviced by route: "
                                  , T.pack route
                                  , " by agency: "
                                  , T.pack agency])
            >> return False
        dlist ->
          let uniqueStops = S.toList . S.fromList . filter (\x -> stopTag x == T.pack stop) . concat . fmap directionStops $ dlist
           in do
            TIO.putStrLn  (T.concat ["Successfully validated:\n\tStop: "
                                     , T.pack stop
                                     , "\n\t"
                                     , stopTitle . head $ uniqueStops
                                     , "\n\nServiced by:\n\t"
                                     , T.concat . intersperse "\n\t" . fmap directionTitle $ dlist])
            return True


data NextPredResult = NPR [TTCPrediction]
                    | NPError
                    deriving (Eq, Ord, Show)

printNPRPlain :: NextPredResult -> IO ()
printNPRPlain (NPR []) = TIO.putStrLn "No results found"
printNPRPlain (NPR predList) = TIO.putStrLn . plainPrintPredictionsBySoonest $ predList
printNPRPlain NPError = TIO.putStrLn "Error: unable to retrieve results"


printNPRPretty :: NextPredResult -> IO ()
printNPRPretty (NPR []) = TIO.putStrLn "No results found"
printNPRPretty (NPR predList) = TIO.putStrLn . prettyPrintPredictionsBySoonest $ predList
printNPRPretty NPError = TIO.putStrLn "Error: unable to retrieve results"

resultsErrorWaybar :: WaybarJSON
resultsErrorWaybar = WaybarJSON "ERROR" "" "Error: Unable to retrieve results"

printNPRWaybar :: NextPredResult -> IO ()
printNPRWaybar (NPR predList) = TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode . waybarPrintPredictionsBySoonest $ predList
printNPRWaybar NPError = TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $ resultsErrorWaybar

printNPRPolybar :: NextPredResult -> IO ()
printNPRPolybar (NPR predList) = TIO.putStrLn . singleSoonestPrediction $ predList
printNPRPolybar NPError = TIO.putStrLn "ERROR"

nextPredictionToNPResult :: String -> String -> String -> IO NextPredResult
nextPredictionToNPResult agency rtTag stpTag = do
  preds <- getPredictionsByRouteAndStop agency rtTag stpTag
  case preds of
    Nothing -> return NPError
    Just pList -> return $ NPR pList

configPreferredFormatToOutputFormat :: Maybe Text -> Maybe ARGS.OutputFormat
configPreferredFormatToOutputFormat Nothing = Nothing
configPreferredFormatToOutputFormat (Just txt) =
  case (T.toCaseFold . T.strip) txt of
    "pretty" -> Just ARGS.Pretty
    "plain" -> Just ARGS.Plain
    "waybar" -> Just ARGS.Waybar
    "polybar" -> Just ARGS.Polybar
    _ -> Nothing

correctPredictionResultPrinter :: Maybe ARGS.OutputFormat -> NextPredResult -> IO ()
correctPredictionResultPrinter Nothing = correctPredictionResultPrinter (Just ARGS.Pretty)
correctPredictionResultPrinter (Just ARGS.Pretty) = printNPRPretty
correctPredictionResultPrinter (Just ARGS.Plain) = printNPRPlain
correctPredictionResultPrinter (Just ARGS.Waybar) = printNPRWaybar
correctPredictionResultPrinter (Just ARGS.Polybar) = printNPRPolybar

runNextPredictionCommand :: String -> Maybe String -> Maybe String -> Maybe ARGS.OutputFormat -> IO ()
runNextPredictionCommand _ Nothing Nothing Nothing = do
  cf <- getConfigFile
  case cf of
    Nothing -> do
      writeToStdErr "Insufficient arguments provided, and no config file saved. Unable to proceed."
      printNPRPretty NPError
    Just conf ->
      runNextPredictionCommand (T.unpack . cf_agency $ conf)
                               (Just . T.unpack $ cf_route conf)
                               (Just . T.unpack $ cf_stop conf)
                               (configPreferredFormatToOutputFormat . cf_preferred_output_format $ conf)
runNextPredictionCommand _ Nothing Nothing outformat = do
  cf <- getConfigFile
  case cf of
    Nothing -> do
      writeToStdErr "Insufficient arguments provided, and no config file saved. Unable to proceed."
      correctPredictionResultPrinter outformat $  NPError
    Just conf ->
      runNextPredictionCommand (T.unpack . cf_agency $ conf)
                               (Just . T.unpack $ cf_route conf)
                               (Just . T.unpack $ cf_stop conf)
                               outformat
runNextPredictionCommand _ Nothing _ outformat = do
  writeToStdErr "Route argument -r missing. Unable to proceed"
  correctPredictionResultPrinter outformat NPError
runNextPredictionCommand _ _ Nothing outformat = do
  writeToStdErr "Stop argument -s missing. Unable to proceed"
  correctPredictionResultPrinter outformat NPError
runNextPredictionCommand a (Just r) (Just s) outformat = do
  result <- nextPredictionToNPResult a r s
  correctPredictionResultPrinter outformat $ result

handleArgs :: ARGS.SwankyTTC -> IO ()
handleArgs (ARGS.Search a r s outformat ) =
  case outformat of
    Nothing -> runSearchCommand a r s ARGS.Pretty
    Just outf -> runSearchCommand a r s outf
handleArgs (ARGS.Next a r s outformat) = runNextPredictionCommand a r s outformat
handleArgs x@(_) = print x

main :: IO ()
main = do
  ARGS.runSwankybarTTCCli >>= handleArgs
