{-# LANGUAGE DeriveDataTypeable #-}

module CmdArgs where

import System.Console.CmdArgs

version :: String
version = "0.1.9.0"

data OutputFormat = Pretty
                  | Plain
                  | Waybar
                  | Polybar
                  deriving (Eq, Typeable, Data, Show)

data SwankyTTC = Next {agency :: String, route :: Maybe String, stop :: Maybe String, output_format :: Maybe OutputFormat}
               | Search {agency :: String, route :: Maybe String, stop :: Maybe String, output_format :: Maybe OutputFormat}
               deriving (Data, Typeable, Show, Eq)

next = Next {
    agency = "ttc" &= help "The agency to search under. Default \"ttc\"" &= opt "ttc"
  , route = def &= help "The route to get production's for (e.g. 501)"
  , stop = def &= help "The stop ID of the route to fetch the next prediction for"
  , output_format = def &= help "Display results in this format. Accepted values [pretty,plain,waybar,polybar]. Default \"pretty\"" &= name "O"
  } &= help "Fetch the next upcoming ETA for a route and stop"
    &= auto

search = Search {
    agency = "ttc" &= help "The agency to search under. Defaults to \"ttc\"" &= opt "ttc"
  , route = def &= help "Retrieve the stops on this route (e.g. 501)"
  , stop = def &= help "Selects a specific stop for a given route. Requires a -r/--route specified"
  , output_format = def &= help "Display search results in this format. Accepted values: [pretty,plain]. Default \"pretty\"" &= name "O"
  } &= help "Retrieve specific route IDs and stop IDs to be used to fetch next ETAs"


swankybarTTCCli = cmdArgsMode $ modes [next, search]
                                &= helpArg [help "Queries the TTC's APIs and fetches the ETAs for a route and stop", name "h"]
                                &= program "swankybar-ttc"
                                &= summary ("swankybar-ttc v." ++ version ++ "\nA CLI-based tool that fetches the TTC's next ETA")

runSwankybarTTCCli = cmdArgsRun swankybarTTCCli

