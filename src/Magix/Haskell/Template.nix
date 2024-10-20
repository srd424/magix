{
  pkgs ? import <nixpkgs> { },

  makeWrapper ? pkgs.makeWrapper,
}:

pkgs.stdenv.mkDerivation {
  name = "__SCRIPT_NAME__";
  src = __SCRIPT_SOURCE__;

  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (ps: with ps; [ __HASKELL_PACKAGES__ ]))
  ];
  buildPhase = ''
    SRC=__SCRIPT_NAME__
    OUT=bin/__SCRIPT_NAME__

    mkdir bin
    mv $SRC $SRC.hs; ghc __HASKELL_GHC_FLAGS__ -o $OUT $SRC.hs
  '';

  installPhase = ''
    mkdir -p $out
    mv bin $out/bin

    source ${makeWrapper}/nix-support/setup-hook
    wrapProgram $out/bin/__SCRIPT_NAME__ --argv0 __SCRIPT_NAME__'';
}
