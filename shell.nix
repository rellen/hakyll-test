{
  pkgs ? import <nixpkgs> { },
  nixpkgs ? <nixpkgs>,
}:
let
  inherit (pkgs.lib) optional optionals;
  ghc = pkgs.haskellPackages.ghcWithPackages (
    pkgs: with pkgs; [
      cabal-install
      haskell-language-server
    ]
  );
in
pkgs.mkShell rec {
  name = "haskell shell";
  buildInputs =
    with pkgs;
    [
      ghc
      zlib
      treefmt
      nixfmt-rfc-style
      ormolu
      nodePackages.prettier

      imagemagick
    ]
    ++ optional stdenv.isLinux inotify-tools
    ++ optionals stdenv.isDarwin (
      with darwin.apple_sdk.frameworks;
      [
        CoreFoundation
        CoreServices
        Cocoa
      ]
    );
}
