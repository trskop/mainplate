{ pkgs ? import <nixpkgs> { } }:

let
  epkgs = pkgs.haskellPackages.extend (self: super: {
    host-and-port = pkgs.callPackage (pkgs.fetchFromGitHub {
      owner = "trskop";
      repo = "host-and-port";
      rev = "67a38bd04c7385542706da7d80822b1a35e76322";
      sha256 = "1sf01f85jv49phrnjwbi5b2f0hy663izsz4y0lv6678c1llsikk6";
    }) { };
  });

in epkgs.callCabal2nix "mainplate" ./. { }
