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
    mkdir bin

    script_source_hs="__SCRIPT_NAME__.hs"
    ln -s "$src" "$script_source_hs"
    ghc __GHC_FLAGS__ -o "bin/__SCRIPT_NAME__" "$script_source_hs"
  '';

  installPhase = ''
    mkdir -p $out
    mv bin $out/

    wrapProgram "$out/bin/__SCRIPT_NAME__"'';
}
