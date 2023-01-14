self: super: {
  neil = super.neil.overrideAttrs (oldAttrs: rec {
    src = super.fetchFromGitHub {
      owner = "babashka";
      repo = "neil";
      rev = "3b61436e3";
      hash = "sha256-mrvxfSHI1nIQyhiMLgFUh8dnzgOv16DYEKwjIoQsQo8=";
    };
  });
}
