# from the sample app in miso: https://github.com/dmjio/miso/blob/master/sample-app/default.nix
with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.7.1.tar.gz";
}) {});
pkgs.haskell.packages.ghcjs.callCabal2nix "capybara" ./. {}