{
  pkgs ? import <nixpkgs> { },
}:

let
  src = __SCRIPT_SOURCE__;
in
pkgs.writeShellApplication {
  name = "__SCRIPT_NAME__";

  runtimeInputs = with pkgs; [ __RUNTIME_INPUTS__ ];

  text = ''
    ${pkgs.bash}/bin/bash ${src} "$@"
  '';
}
