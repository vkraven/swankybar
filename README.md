# swankybar

Sway + Waybar custom plugin to toggle Adaptive Sync from Waybar.

Made this custom plugin because Sway with adaptive sync on causes my monitors to flicker, so I only want to use it when gaming. But I can never remember the magical invocation and `swaymsg --help` is not very helpful.

Static-linked Haskell release binary lovingly built with Nix flakes! See `flake-static.nix`.

## Requirements

- Sway
- Waybar > v 0.9.5
- (Optional) DBus notifications through `org.freedesktop.Notifications`
- A font with the following Glyphs: 󰓦 󱆢 󰓨 󰓧

## Installation

 1. Download `swankybar-as`
 2. `chmod +x swankybar-as`
 3. Add this to your Waybar config (usually in `~/.config/waybar/config`)
 
 ```json
 "custom/swankybar-as" : {
   "format": "{}",
   "format-alt": "{}",
   "on-click": "PATH/TO/swankybar-as toggle", // UPDATE THIS
   "on-click-middle": "PATH/TO/swankybar-as toggle --all", // UPDATE THIS
   "on-click-right": "PATH/TO/swankybar-as", // UPDATE THIS
   "return-type": "json",
   "tooltip": true,
   "exec": "PATH/TO/swankybar-as", // UPDATE THIS
   "exec-on-event": true,
   "interval": 60 // Set this to whatever you like
 }
 ```
 
## Installation on NixOS

 1. Add to an overlay with 
 ```nix
 super.haskellPackages.callPackage
 ```
