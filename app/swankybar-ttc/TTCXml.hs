{-# LANGUAGE OverloadedStrings #-}

module TTCXml where

import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup
import Control.Monad.Reader
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)

data TTCRoute = TTCRoute {
    routeTag :: Text
  , routeTitle :: Text
  } deriving (Eq, Ord, Show)

data TTCStop = TTCStop {
    stopTag :: Text
  , stopTitle :: Text
  , stopId :: Maybe Text
  } deriving (Eq, Ord, Show)

data TTCDirection = TTCDirection {
    directionTag :: Text
  , directionTitle :: Text
  , directionName :: Text
  , directionBranch :: Text
  , directionStops :: [TTCStop]
  } deriving (Eq, Ord, Show)

data TTCPrediction = TTCPrediction {
    predictionSeconds :: Integer
  , predictionMinutes :: Integer
  , predictionIsDeparture :: Maybe Bool
  , predictionAffectedByLayover :: Maybe Bool
  , predictionBranch :: Text
  , predictionDirection :: TTCDirection
  , predictionVehicle :: Text
  , predictionBlock :: Text
  , predictionTripTag :: Text
  , predictionRoute :: TTCRoute
  , predictionStop :: TTCStop
  } deriving (Eq, Ord, Show)

data PredictionParsingPartialEnv = PredictionParsingPartialEnv {
    partialEnvStop :: TTCStop
  , partialEnvRoute :: TTCRoute
  } deriving (Eq, Ord, Show)

data PredictionParsingEnv = PredictionParsingEnv {
    envStop :: TTCStop
  , envRoute :: TTCRoute
  , envDirTitle :: Text
  } deriving (Eq, Ord, Show)

predictionParsingPartialToFullEnv :: PredictionParsingPartialEnv -> Text -> PredictionParsingEnv
predictionParsingPartialToFullEnv (PredictionParsingPartialEnv s r) =
  PredictionParsingEnv s r

getTagBody :: [Tag Text] -> [Tag Text]
getTagBody = dropTagOpenBodyPrefix . takeWhile (not . isTagCloseName "body") . dropWhile (not . isTagOpenName "body")

dropTagOpenBodyPrefix :: [Tag Text] -> [Tag Text]
dropTagOpenBodyPrefix [] = []
dropTagOpenBodyPrefix y@(x:xs) = if isTagOpenName "body" x
                                  then xs
                                  else y

findPredictionsTagOpenStart :: [Tag Text] -> [Tag Text]
findPredictionsTagOpenStart = dropWhile (not . isTagOpenName "predictions")

constructRouteFromPredictionsTagOpen :: [Tag Text] -> Maybe TTCRoute
constructRouteFromPredictionsTagOpen [] = Nothing
constructRouteFromPredictionsTagOpen (ot@(TagOpen "predictions" _):_) =
  Just $ TTCRoute (fromAttrib "routeTag" ot)
                  (fromAttrib "routeTitle" ot)
constructRouteFromPredictionsTagOpen _ = Nothing

constructStopFromPredictionsTagOpen :: [Tag Text] -> Maybe TTCStop
constructStopFromPredictionsTagOpen [] = Nothing
constructStopFromPredictionsTagOpen (ot@(TagOpen "predictions" _):_) =
  Just $ TTCStop (fromAttrib "stopTag" ot)
                 (fromAttrib "stopTitle" ot)
                 Nothing

cleanTags :: Text -> [Tag Text] -> [Tag Text]
cleanTags leavingName = takeWhile (not . isTagCloseName leavingName) . dropWhile (not . isTagOpenName leavingName)

predictionsTagBodyToDirectionChunks :: [Tag Text] -> [[Tag Text]]
predictionsTagBodyToDirectionChunks = fmap (cleanTags "direction") . partitions (isTagOpenName "direction")

extractDirectionTitleFromPredictionDirection :: [Tag Text] -> Maybe Text
extractDirectionTitleFromPredictionDirection [] = Nothing
extractDirectionTitleFromPredictionDirection (ot@(TagOpen "direction" _): _) =
  Just $ fromAttrib "title" ot
extractDirectionTitleFromPredictionDirection _ = Nothing

directionTagBodyToPredictionChunks :: [Tag Text] -> [[Tag Text]]
directionTagBodyToPredictionChunks = fmap (cleanTags "prediction") . partitions (isTagOpenName "prediction")

readMaybeBoolString :: Text -> Maybe Bool
readMaybeBoolString = readMaybe . T.unpack . T.toTitle

predictionsTagToTTCPrediction :: [Tag Text] -> Reader PredictionParsingEnv (Maybe TTCPrediction)
predictionsTagToTTCPrediction [] = return Nothing
predictionsTagToTTCPrediction (ot@(TagOpen "prediction" _):_) = do
  pStop <- asks envStop
  pRoute <- asks envRoute
  pDirTitle <- asks envDirTitle
  let pDir = TTCDirection (fromAttrib "dirTag" ot)
                          pDirTitle
                          ""
                          (fromAttrib "branch" ot)
                          [pStop]
      maybeSecs = readMaybe . T.unpack $ fromAttrib "seconds" ot
      maybeMins = readMaybe . T.unpack  $ fromAttrib "minutes" ot
      maybeIsDeparture = readMaybeBoolString (fromAttrib "isDeparture" ot)
      maybeAffectedByLayover = readMaybeBoolString (fromAttrib "affectedByLayover" ot)
   in if isNothing maybeSecs || isNothing maybeMins
        then return Nothing
        else return . Just $
               TTCPrediction (fromJust maybeSecs)
                             (fromJust maybeMins)
                             maybeIsDeparture
                             maybeAffectedByLayover
                             (fromAttrib "branch" ot)
                             pDir
                             (fromAttrib "vehicle" ot)
                             (fromAttrib "block" ot)
                             (fromAttrib "tripTag" ot)
                             pRoute
                             pStop
predictionsTagToTTCPrediction _ = return Nothing

tagBodyToNamedChunks :: Text -> [Tag Text] -> [[Tag Text]]
tagBodyToNamedChunks tagName =
  fmap (cleanTags tagName) . partitions (isTagOpenName tagName)

directionTagBodyToTTCPrediction :: [Tag Text] -> Reader PredictionParsingPartialEnv [Maybe TTCPrediction]
directionTagBodyToTTCPrediction [] = return []
directionTagBodyToTTCPrediction dirBlock@(ot@(TagOpen "direction" _):_) = do
  partialEnv <- ask
  case extractDirectionTitleFromPredictionDirection dirBlock of
    Nothing -> return []
    Just dirTitle ->
      let predBlocks = directionTagBodyToPredictionChunks dirBlock
          newEnv = predictionParsingPartialToFullEnv partialEnv dirTitle
       in return $ runReader (traverse predictionsTagToTTCPrediction predBlocks) newEnv

xmlToTTCPredictions :: Text -> Maybe [TTCPrediction]
xmlToTTCPredictions txt =
  let tags = parseTags txt
      predTags = findPredictionsTagOpenStart . getTagBody $ tags
      directionTags = predictionsTagBodyToDirectionChunks predTags
   in do
    maybeStop <- constructStopFromPredictionsTagOpen predTags
    maybeRoute <- constructRouteFromPredictionsTagOpen predTags
    let partialEnv = PredictionParsingPartialEnv maybeStop maybeRoute
        out = runReader (traverse directionTagBodyToTTCPrediction directionTags) partialEnv
    sequence . concat $ out

routeTagBodyToTTCRoute :: [Tag Text] -> Maybe TTCRoute
routeTagBodyToTTCRoute [] = Nothing
routeTagBodyToTTCRoute (ot@(TagOpen "route" _):_) =
  Just $ TTCRoute (fromAttrib "tag" ot) (fromAttrib "title" ot)
routeTagBodyToTTCRoute _ = Nothing

stopTagBodyToTTCStop :: [Tag Text] -> Maybe TTCStop
stopTagBodyToTTCStop [] = Nothing
stopTagBodyToTTCStop (ot@(TagOpen "stop" _):_) =
  Just $ TTCStop (fromAttrib "tag" ot)
                 (fromAttrib "title" ot)
                 (if fromAttrib "stopId" ot == "" then Nothing else Just (fromAttrib "stopId" ot))

lookupStopList :: Text -> Reader [TTCStop] (Maybe TTCStop)
lookupStopList tagName = do
  stopList <- ask
  case filter (\x -> stopTag x == tagName) stopList of
    [] -> return Nothing
    (x:_) -> return $ Just x

innerStopTagsToTTCStop :: [Tag Text] -> Reader [TTCStop] (Maybe TTCStop)
innerStopTagsToTTCStop [] = return Nothing
innerStopTagsToTTCStop (ot@(TagOpen "stop" _):_) =
  case (fromAttrib "tag" ot) of
    "" -> return Nothing
    tName -> lookupStopList tName
innerStopTagsToTTCStop _ = return Nothing

directionTagBodyToTTCDirection :: [TTCStop] -> [Tag Text] -> Maybe TTCDirection
directionTagBodyToTTCDirection _ [] = Nothing
directionTagBodyToTTCDirection stopList (ot@(TagOpen "direction" _):remaining) =
  let innerStops = tagBodyToNamedChunks "stop" remaining
      innerTTCStops = runReader (traverse innerStopTagsToTTCStop innerStops) stopList
   in
    case sequence innerTTCStops of
        Nothing -> Nothing
        Just its -> Just $ TTCDirection (fromAttrib "tag" ot)
                                        (fromAttrib "title" ot)
                                        (fromAttrib "name" ot)
                                        (fromAttrib "branch" ot)
                                        its
directionTagBodyToTTCDirection _ _ = Nothing


xmlToTTCDirections :: Text -> Maybe [TTCDirection]
xmlToTTCDirections xml = do
  stopList <- xmlToTTCStops xml
  traverse (directionTagBodyToTTCDirection stopList) . tagBodyToNamedChunks "direction" . parseTags $ xml

xmlToTTCStops :: Text -> Maybe [TTCStop]
xmlToTTCStops =
  traverse stopTagBodyToTTCStop . tagBodyToNamedChunks "stop" . takeWhile (not . isTagOpenName "direction") . parseTags

xmlToTTCRoutes :: Text -> Maybe [TTCRoute]
xmlToTTCRoutes =
  traverse routeTagBodyToTTCRoute . tagBodyToNamedChunks "route" . parseTags
