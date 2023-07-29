{-# LANGUAGE DeriveDataTypeable #-}

module CmdArgs where

import System.Console.CmdArgs

version :: String
version = "0.1.9.0"

data SwankyCpuGov = Display
                    deriving (Data, Typeable, Show, Eq)

display = Display &= help "Displays the current CPU frequency scaling governor"
                  &= auto

swankybarCpuGovCli = cmdArgsMode $ modes [display]
                                   &= helpArg [help "Displays the system's current CPU frequency governor", name "h"]
                                   &= program "swankybar-cpugov"
                                   &= summary ("swankybar-cpugov v." ++ version ++ "\nHelper tool to display the dominant CPU governor in a nice Waybar JSON")

runSwankybarCpuGovCli = cmdArgsRun swankybarCpuGovCli
