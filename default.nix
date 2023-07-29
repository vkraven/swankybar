{ mkDerivation, aeson, base, binary, bytestring, cmdargs
, containers, dbus, directory, fdo-notify, lib, mtl, network
, process, text, transformers, unix
}:
mkDerivation {
  pname = "swankybar";
  version = "0.1.9.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base binary bytestring cmdargs containers dbus directory
    fdo-notify mtl network process text transformers unix
  ];
  description = "A Waybar custom plugin to toggle Adaptive Sync in Sway and display CPU scaling governors";
  license = lib.licenses.gpl3Only;
}
