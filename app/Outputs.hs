{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Outputs where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

data OutputMode = OutputMode {
    om_width :: Integer
  , om_height :: Integer
  , om_refresh :: Integer
  , om_picture_aspect_ratio :: Text
  } deriving (Eq, Show, Ord, Generic)

instance FromJSON OutputMode where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3
    }

instance ToJSON OutputMode where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 3
    }
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 3
    }

data Output = Output {
    o_id :: Integer
  , o_type :: Text
  , o_orientation :: Text
  , o_percent :: Rational
  , o_urgent :: Bool
  , o_name :: Text
  , o_primary :: Bool
  , o_make :: Text
  , o_model :: Text
  , o_serial :: Text
  , o_modes :: [OutputMode]
  , o_active :: Bool
  , o_dpms :: Bool
  , o_power :: Bool
  , o_scale :: Rational
  , o_scale_filter :: Text
  , o_adaptive_sync_status :: Text
  , o_current_mode :: OutputMode
  , o_focused :: Bool
  , o_subpixel_hinting :: Text
  } deriving (Eq, Show, Ord, Generic)

instance FromJSON Output where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 2
    }

instance ToJSON Output where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 2
    }
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 2
    }

data WaybarJSON = WaybarJSON {
  wj_text :: String,
  wj_alt :: String,
  wj_tooltip :: String
  } deriving (Eq, Show, Generic)

instance ToJSON WaybarJSON where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 3
    }

instance FromJSON WaybarJSON where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3
    }
