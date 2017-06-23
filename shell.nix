{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, directory
      , filepath, filestore, hslogger, mtl, network, network-uri
      , pandoc, pandoc-types, process, SHA, stdenv, utf8-string
      , process-streaming, pipes-transduce
      }:
      mkDerivation {
        pname = "jrg_wiki_pandoc_converter_filters";
        version = "0.0.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring directory filepath filestore
          hslogger mtl network network-uri pandoc pandoc-types process SHA
          utf8-string process-streaming pipes-transduce
        ];
        executableSystemDepends = with pkgs; [ 
              curl
              wget
              graphviz
              plantuml
              ditaa
              jre
              diagrams-builder
              coreutils # For `realpath` used in `./run.sh`.
        ];

        license = "GPL";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

