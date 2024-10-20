{
  pkgs ? import <nixpkgs> { },
  makeWrapper ? pkgs.makeWrapper,
}:

pkgs.stdenv.mkDerivation {
  name = "__SCRIPT_NAME__";

  src = __SCRIPT_SOURCE__;
  dontUnpack = true;

  nativeBuildInputs = [ makeWrapper ];

  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (ps: with ps; [ __HASKELL_PACKAGES__ ]))
  ];

  buildPhase = ''
    OUT=bin/__SCRIPT_NAME__
    SCRIPT_SOURCE_HS=__SCRIPT_NAME__.hs

    mkdir bin
    ln -s $src $SCRIPT_SOURCE_HS
    ghc __HASKELL_GHC_FLAGS__ -o $OUT $SCRIPT_SOURCE_HS
  '';

  installPhase = ''
    mkdir -p $out
    mv bin $out/bin

    wrapProgram $out/bin/__SCRIPT_NAME__'';
}
