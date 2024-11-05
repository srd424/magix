{
  pkgs ? import <nixpkgs> { },
}:

let
  pythonEnv = pkgs.python3.withPackages (ps: with ps; [ __PYTHON_PACKAGES__ ]);
  scriptWithOriginalName = pkgs.writeScript "replace-command-name-wrapper" ''
    from sys import argv
    argv[0] = "__SCRIPT_NAME__"
    exec(open("${__SCRIPT_SOURCE__}").read())
  '';
in
pkgs.writeShellApplication {
  name = "__SCRIPT_NAME__";

  text = ''
    ${pythonEnv}/bin/python ${scriptWithOriginalName} "$@"
  '';
}
