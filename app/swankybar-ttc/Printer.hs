{-# LANGUAGE OverloadedStrings #-}

module Printer where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Tabl
import Data.List (sortOn, intersperse)
import Text.Read (readMaybe)
import TTCXml
import qualified Data.Set as S
import Outputs (WaybarJSON (WaybarJSON))

prettyPrintRouteList :: [TTCRoute] -> Text
prettyPrintRouteList rlist =
  tabl EnvAscii (DecorUnion [DecorOuter, DecorOnly [1]])
       DecorAll [AlignCentre, AlignLeft] textPlusSorted
    where sorted = sortOn ((readMaybe . T.unpack . routeTag) :: TTCRoute -> Maybe Integer) rlist
          routeToList (TTCRoute rt rtitle) = [rt, rtitle]
          textPlusSorted = [["ID", "Route Name"]] <> (routeToList <$> sorted)

plainPrintRouteList :: [TTCRoute] -> Text
plainPrintRouteList =
  T.concat . intersperse "\n" . fmap routeToPlain
    where routeToPlain (TTCRoute rt rtitle) = T.concat [rt, ": ", rtitle]

prettyPrintDirectionHeader :: TTCDirection -> Text
prettyPrintDirectionHeader (TTCDirection _ title name branch _) =
  tabl EnvAscii DecorOuter DecorNone [AlignCentre]
    [[T.concat [name, "bound"]], [branch], [title]]

prettyPrintDirectionStops :: TTCDirection -> Text
prettyPrintDirectionStops (TTCDirection _ _ name branch stoplist) =
  tabl EnvAscii (DecorUnion [DecorOuter, DecorOnly [1]])
       DecorAll [AlignLeft, AlignLeft, AlignLeft, AlignLeft]
       text
   where stopToRow (TTCStop stag stitle _) = [name, stag, branch, stitle]
         text = [["Direction", "Stop ID", "Branch", "Stop Name"]] <> (fmap stopToRow stoplist)


prettyPrintDirection :: TTCDirection -> Text
prettyPrintDirection dir =
  T.concat [prettyPrintDirectionHeader dir, "\n", prettyPrintDirectionStops dir]

prettyPrintDirections :: [TTCDirection] -> Text
prettyPrintDirections =
  T.concat . intersperse "\n\n" . fmap prettyPrintDirection


plainPrintDirection :: TTCDirection -> Text
plainPrintDirection (TTCDirection _ title name branch stopList) =
  T.concat [title, "\n", T.pack $ take maxlen (repeat '-'), "\n", stopText]
    where makeStopText (TTCStop stag stitle _) = T.concat [name
                                                          , "bound - "
                                                          , branch
                                                          , " - "
                                                          , stag
                                                          , " - "
                                                          , stitle]
          stopTextList = makeStopText <$> stopList
          maxlen = maximum $ [T.length title]  <> (T.length <$> stopTextList)
          stopText = T.concat . intersperse "\n" $ stopTextList

plainPrintDirections :: [TTCDirection] -> Text
plainPrintDirections =
  T.concat . intersperse "\n\n" . fmap plainPrintDirection

secondsToHMS :: Integer -> Text
secondsToHMS secs =
  let hours = secs `div` (60 * 60)
      remMinutesInSeconds = secs `rem` (60 * 60)
      remMins = remMinutesInSeconds `div` 60
      remSecs = remMinutesInSeconds `rem` 60
   in if hours > 0
        then T.concat [(T.pack . show $ hours)
                      , " hrs "
                      , (T.pack . show $ remMins)
                      , " mins "
                      , (T.pack . show $ remSecs)
                      , " secs"]
        else if remMins > 0
                then T.concat [(T.pack . show $ remMins)
                              , " mins "
                              , (T.pack . show $ remSecs)
                              , " secs"]
                else T.concat [(T.pack . show $ remSecs), " secs"]

prettyPrintPredictionsSoonestHeader :: [TTCPrediction] -> Text
prettyPrintPredictionsSoonestHeader [] = ""
prettyPrintPredictionsSoonestHeader predList@(first:rest) =
  tabl EnvAscii (DecorUnion [DecorOuter, DecorOnly [2]]) DecorNone [AlignCentre]
       ([ [T.concat ["Stop ID: ", stopTag .  predictionStop $ first]]
       , [stopTitle . predictionStop $ first]] <> servicedBy)
    where uniqueDirs = S.toList . S.fromList $ predictionDirection <$> predList
          servicedBy = (\x -> [x]) . directionTitle <$> uniqueDirs

prettyPrintPredictionsSoonestBody :: [TTCPrediction] -> Text
prettyPrintPredictionsSoonestBody predList =
  tabl EnvAscii (DecorUnion [DecorOuter, DecorOnly [1]]) DecorAll
       [AlignLeft, AlignLeft, AlignLeft]
       ([["Branch", "Vehicle #", "ETA."]] <> preds)
    where makePredRow p = [predictionBranch p, predictionVehicle p, secondsToHMS . predictionSeconds $ p]
          preds = makePredRow <$> predList

prettyPrintPredictionsBySoonest :: [TTCPrediction] -> Text
prettyPrintPredictionsBySoonest [] = "No results found"
prettyPrintPredictionsBySoonest predList =
  T.concat [header, "\n\n", body]
    where header = prettyPrintPredictionsSoonestHeader predList
          body = prettyPrintPredictionsSoonestBody . sortOn predictionSeconds $ predList

plainPrintPredictionsBySoonest :: [TTCPrediction] -> Text
plainPrintPredictionsBySoonest [] = "No results found"
plainPrintPredictionsBySoonest predList =
  T.concat [header
           , "\n"
           , T.pack (replicate maxlen '-')
           , "\n"
           , body]
    where header = T.concat ["Stop "
                            , stopTag . predictionStop . head $ predList
                            , " - "
                            , stopTitle . predictionStop . head $ predList]
          bodyLines  = fmap makePlainPredictionLine . sortOn predictionSeconds $ predList
          maxlen = maximum $ [T.length header] <> (T.length <$> bodyLines)
          body = T.concat . intersperse "\n" $ bodyLines


makePlainPredictionLine :: TTCPrediction -> Text
makePlainPredictionLine p = T.concat [ predictionBranch p
                                     , " - "
                                     , predictionVehicle p
                                     , ": "
                                     , secondsToHMS . predictionSeconds $ p]

secondsToHOrMOrS :: Integer -> Text
secondsToHOrMOrS secs =
  let hours = secs `div` (60 * 60)
      remMinutesInSeconds = secs `rem` (60 * 60)
      remMins = remMinutesInSeconds `div` 60
      remSecs = remMinutesInSeconds `rem` 60
   in if hours > 0
         then T.concat [T.pack . show $ hours
                       , " hrs"]
         else if remMins > 0
                 then T.concat [T.pack . show $ remMins
                               , " mins"]
                 else T.concat [T.pack . show $ secs
                               , "secs"]

singleSoonestPrediction :: [TTCPrediction] -> Text
singleSoonestPrediction [] = "ERROR"
singleSoonestPrediction predList =
  T.concat [branch, ": ", time]
    where soonest = head $ sortOn predictionSeconds predList
          branch = predictionBranch soonest
          time = secondsToHOrMOrS . predictionSeconds $ soonest

waybarPrintPredictionsBySoonest :: [TTCPrediction] -> WaybarJSON
waybarPrintPredictionsBySoonest predList =
  WaybarJSON (T.unpack . singleSoonestPrediction $ predList)
             ""
             (T.unpack . T.concat . intersperse "\n" . fmap makePlainPredictionLine . sortOn predictionSeconds $ predList)
