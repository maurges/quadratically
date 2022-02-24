let
  pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-stable-18-03";
    url = "https://github.com/nixos/nixpkgs/";
    # Commit hash for nixos-unstable as of 2018-09-12
    # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
    ref = "refs/tags/18.03";
    rev = "4f57714e03b9e7fdc165b3a4aa8bfa0463afc027";
  }) {};
in pkgs.haskell.packages.ghc841.callPackage ./derivation.nix { }
