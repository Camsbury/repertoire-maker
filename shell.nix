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
    ];
    shellHooks = ''
      export PYTHON_HOME=${python-custom}
    '';
  }
