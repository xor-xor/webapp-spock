{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, stdenv, base, Spock, lucid, postgresql-simple,
        wai-middleware-static, users, hspec, hspec-wai, hlint, ghc-mod,
        configurator
      }:
      mkDerivation {
        pname = "webapp-haskell";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        buildDepends = [
            base Spock lucid postgresql-simple wai-middleware-static
            users hspec hspec-wai hlint ghc-mod configurator
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
