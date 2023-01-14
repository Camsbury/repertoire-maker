let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
in
  import (fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    rev = "5f588eb4a95"; # pin
    hash = "sha256-T/cUx44aYDuLMFfaiVpMdTjL4kpG7bh0VkN6JEM78/E=";
})
