with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.7.1.tar.gz";
}) {});
let
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. { miso = miso-jsaddle; };
  release = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. {};
in
{
  dev = dev.overrideAttrs (old: {
    postInstall = ''
      cp -r ${old.src}/static/* $out/bin/app.jsexe
    '';
  });
  release = release.overrideAttrs (old: {
    postInstall = ''
      cp -r ${old.src}/static/* $out/bin/app.jsexe
    '';
  });
  inherit pkgs;
}