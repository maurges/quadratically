let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  name = "cabal-test";
  buildInputs = with pkgs; [
    ghc cabal-install
  ];

  shellHook = ''
    # c++ binary causes internal panics in GHC, see
    # https://gitlab.haskell.org/ghc/ghc/-/issues/16590

    found_cxx="$(cc --print-file-name 'c++')"
    # if not found, would print the name back
    if [[ "$found_cxx" != "c++" ]]; then
        # check if it's a nix symlink
        case $(dirname "$found_cxx") in
            /nix/store/*/bin)
                # it's a symlink local to the clang-wrapper in the current shell, so it's safe to remove
                rm "$found_cxx"
                ;;
        esac
    fi
  '';
}
