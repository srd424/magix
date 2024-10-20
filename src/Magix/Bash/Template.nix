{
  pkgs ? import <nixpkgs> { },

  makeWrapper ? pkgs.makeWrapper,
}:

pkgs.stdenv.mkDerivation {
  name = "__SCRIPT_NAME__";

  src = __SCRIPT_SOURCE__;
  dontUnpack = true;

  nativebuildInputs = [ makeWrapper ];

  runtimeInputs = with pkgs; [ __RUNTIME_INPUTS__ ];
  buildPhase = ''
    OUT=bin/__SCRIPT_NAME__

    mkdir bin
    cp $src $OUT
  '';

  installPhase = ''
    mkdir -p $out
    mv bin $out/bin

    wrapProgram $out/bin/__SCRIPT_NAME__ --argv0 __SCRIPT_NAME__'';
}
