{ mkDerivation, aeson, base, binary, bytestring, cmdargs
, containers, dbus, directory, fdo-notify, filepath, http-client
, http-client-tls, lib, mtl, network, process, tabl, tagsoup, text
, transformers, unix
}:
mkDerivation {
  pname = "swankybar";
  version = "0.1.9.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base binary bytestring cmdargs containers dbus directory
    fdo-notify filepath http-client http-client-tls mtl network process
    tabl tagsoup text transformers unix
  ];
  description = "A Waybar custom plugin to toggle Adaptive Sync in Sway and display CPU scaling governors, and other tools";
  license = lib.licenses.gpl3Only;
}
