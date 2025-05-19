{
  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; };

  outputs = { self, nixpkgs, }:
    let
      forAllSystems = fn:
        let
          systems = [ "x86_64-linux" "aarch64-darwin" ];
          overlays = [ ];
        in nixpkgs.lib.genAttrs systems
        (system: fn (import nixpkgs { inherit system overlays; }));
    in {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [ pkgs.idris2 pkgs.asciinema_3 pkgs.gmp ];
        };
      });

      packages = forAllSystems (pkgs:
        let
          lc3 = pkgs.idris2Packages.buildIdris {
            ipkgName = "lc3";
            src = ./.;
            version = "0.0.1";
            idrisLibraries = [ ];
          };
        in { default = lc3.executable; });
    };
}
