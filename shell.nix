let
  pkgs = import <nixpkgs> {};
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
    ];
    shellHooks = ''
      export PYTHON_HOME=${python-custom}
      export STOCKFISH_PATH=${stockfish}/bin/stockfish
    '';
  }
