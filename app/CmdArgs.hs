{-# LANGUAGE DeriveDataTypeable #-}

module CmdArgs where

import System.Console.CmdArgs

version :: String
version = "0.1.0.0"

data WaybarAdaptive = Toggle { all_ :: Bool }
                    | Display
                    deriving (Data, Typeable, Show, Eq)

display = Display &= help "Displays the current adaptive sync status in Sway"
                  &= auto

toggle = Toggle { all_ = False &= help "Toggles all outputs' adaptive sync\n\nDefaults to false"
                  } &= help "Toggle adaptive sync on or off on the focused display"

swankybarAdaptiveCli = cmdArgsMode $ modes [display, toggle]
                                   &= helpArg [help "Display or toggle adaptive sync in Sway", name "h"]
                                   &= program "swankybar-as"
                                   &= summary ("swankybar-as v." ++ version ++ "\nHelper tool to toggle adaptive sync on and off\nand print a nice Waybar JSON")

runSwankybarAdaptiveCli = cmdArgsRun swankybarAdaptiveCli
