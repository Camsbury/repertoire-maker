let
  pkgs = import <nixpkgs> {
    overlays = [(import ./overlays.nix)];
  };
  # pkgs = import ./pinned.nix {
  #   overlays = [(import ./overlays.nix)];
  # };
  python-custom = (pkgs.python3.withPackages (
        pythonPackages: with pythonPackages; [
          chess
        ]));
in
  with pkgs;
  mkShell {
    buildInputs = [
      clojure
      clj-kondo
      python-custom
      stockfish
      neil
      zstd
    ];
    shellHooks = ''
      export PYTHON_HOME=${python-custom}
      export STOCKFISH_PATH=${stockfish}/bin/stockfish
    '';
  }
