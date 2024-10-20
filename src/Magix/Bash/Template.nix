{
  pkgs ? import <nixpkgs> { },
  makeWrapper ? pkgs.makeWrapper,
}:

pkgs.stdenv.mkDerivation {
  name = "__SCRIPT_NAME__";

  src = __SCRIPT_SOURCE__;
  dontUnpack = true;

  nativeBuildInputs = [ makeWrapper ];

  buildPhase = ''
    OUT=bin/__SCRIPT_NAME__

    mkdir bin
    echo "#!${pkgs.bash}/bin/bash" > $OUT
    cat $src >> $OUT
    chmod +x $OUT
  '';

  installPhase = ''
    mkdir -p $out
    mv bin $out/bin

    wrapProgram $out/bin/__SCRIPT_NAME__ --argv0 __SCRIPT_NAME__ \
      --prefix PATH : ${with pkgs; lib.makeBinPath [ __BASH_RUNTIME_INPUTS__ ]}
  '';
}
