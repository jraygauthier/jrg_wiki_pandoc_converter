/*
The set of packages used throughout this sandbox.

Updating the pinned nixpkgs (the collection of packages this sandbox is based on)
---------------------------------------------------------------------------------

 -  Get a copy of [GitHub - NixOS/nixpkgs: Nix Packages collection](https://github.com/NixOS/nixpkgs).

    ~~~{.bash}
    cd ~/my-dev-dir
    git clone https://github.com/NixOS/nixpkgs my_nixpkgs
    cd my_nixpkgs
    latestUnstableNixpkgsRev=$(curl -L https://nixos.org/channels/nixpkgs-unstable/git-revision)
    git checkout $latestUnstableNixpkgsRev
    ~~~

 -  Update the `pinnedNixpkgs`'s expression so that:

     -  `url`'s value is `echo "https://github.com/NixOS/nixpkgs/archive/$latestUnstableNixpkgsRev.tar.gz"`
     -  `sha256`'s value is:
         `nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/$latestUnstableNixpkgsRev.tar.gz  2>&1 | tail -n 1`

         Which is the hash of the tarball **after** unpacking.

*/

{ usePinnedNixpkgs ? true }:

let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  pinnedNixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/017561209e933f3de086e04211dc48cf37d1ee82.tar.gz";
    # Get this info from the output of: `nix-prefetch-url --unpack $url` where `url` is the above.
    sha256 = "1j42hdr19s7mkfwwff0s3n7530s6y1g6xrz9a98kb0hjd2rk8fm5";
  };

  nixpkgsConfig = {
    allowUnfree = false;
  };

  pkgsOverlay = self: super: {};
  pkgsOverlays = [ pkgsOverlay ];

  pinnedPkgs = import pinnedNixpkgs { config = nixpkgsConfig; overlays = pkgsOverlays; };
  installedPkgs = import <nixpkgs> { config = nixpkgsConfig; overlays = pkgsOverlays; };

  sandboxPkgs = (if usePinnedNixpkgs then pinnedPkgs else installedPkgs);

in

sandboxPkgs
