{
  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-26.05/nixexprs.tar.xz";
    rUtils.url = "git+https://scm.openanalytics.eu/git/oa-r-utils-nix.git";
  };

  outputs = inputs: let
    extrasNotInDescription = pkgs: [
      pkgs.rPackages.devtools
      pkgs.rPackages.languageserver
      pkgs.awscli2
    ];
  in
  {
    devShells = builtins.mapAttrs (
      system: pkgs':
      let
        pkgs = pkgs'.extend (import ./overlay.nix { rUtils = inputs.rUtils; });

        arf = pkgs.rustPlatform.buildRustPackage {
          pname = "arf";
          version = "0.5.3";

          src = pkgs.fetchFromGitHub {
            owner = "eitsupi";
            repo = "arf";
            rev = "v0.5.3";
            hash = "sha256-Ema0s5whJwYY9Bg3HSKf1XFWqANzuoJgpqSVsVKBoWg=";
          };
          doCheck = false; 
          cargoHash = "sha256-yGV/JSqK03bSoHsvbDyg4d+1Zf59nsT8tg9YH4oL7Lg=";
        };

      in
      {
        default = inputs.rUtils.lib.mkRShell {
          inherit pkgs;
          shell_name = "default";
          
          # Using your DESCRIPTION file packages will automatically be installed e.g.:
          descriptionPath = reporting-grofwild/DESCRIPTION;

          # If you want to exclude the Suggests packages:
          # excludeSuggests = true;

          packages = (extrasNotInDescription pkgs) ++ [
            inputs.self.packages.${system}.default
            arf
          ];
          
          # Here you can add commands that will run everytime you enter the
          # development shell
          shellHook = ''
          '';
        };
        test = inputs.rUtils.lib.mkRShell {
          inherit pkgs;
          shell_name = "test";
          packages = (extrasNotInDescription pkgs) ++ [
            inputs.self.packages.${system}.default
          ];
          shellHook = ''
          '';
        };
      }
    ) inputs.nixpkgs.legacyPackages;
    packages = builtins.mapAttrs (
      system: pkgs':
      let
        pkgs = pkgs'.extend (import ./overlay.nix { rUtils = inputs.rUtils; });
      in
      {
        reportingGrofwild = inputs.rUtils.lib.buildRPackage {
          inherit pkgs;
          src = inputs.self + "/reporting-grofwild";
        };
        default = inputs.self.packages.${system}.reportingGrofwild;
      }
    ) inputs.nixpkgs.legacyPackages;
  };
}
