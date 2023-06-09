with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
  sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
}) {});
let
  release = pkgs.haskell.packages.ghcjs86.callCabal2nix "sonny76" ./. {};
in {
  release = release.overrideAttrs (old: {
    postInstall = ''
      cp -r ${old.src}/assets $out/bin/sonny76.jsexe/assets
    '';
  });
  inherit pkgs;
}
