{
  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    rUtils.url = "git+https://scm.openanalytics.eu/git/oa-r-utils-nix.git";
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    rUtils
  }: utils.lib.eachDefaultSystem (
    system:
    let

      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (final: prev: {
            rPackages = prev.rPackages.override {
              overrides = {
                slickR = prev.rPackages.slickR.overrideAttrs (self: nixpkgs.lib.recursiveUpdate self { meta.broken = false; });

                INBOtheme = prev.rPackages.buildRPackage {
                  name = "INBOtheme";
                  src = prev.fetchFromGitHub {
                    owner = "inbo";
                    repo = "INBOtheme";
                    rev = "278ce189737fb147819c5d14850579653ff0c768";
                    hash = "sha256-32dzA3m80DMOxU2vYeRo6+zchiYRJyxMghIqBEP4aWM=";
                  };
                  propagatedBuildInputs = with prev.rPackages; [ assertthat colorspace conflicted ggplot2 scales ];
                };

                aws_s3 = prev.rPackages.buildRPackage {
                  name = "aws_s3";
                  src = prev.fetchFromGitHub {
                    owner = "cloudyr";
                    repo = "aws.s3";
                    rev = "0.3.22";
                    hash = "sha256-Swo2397bDUfcX0h5RKdC+pK78Yx72wVUmFeOWsh3HEA=";
                  };
                  propagatedBuildInputs = with prev.rPackages; [ curl httr xml2 base64enc digest aws_signature ];
                };

              };
            };
          })
        ];
      };

      rpackages = with pkgs.rPackages; [
        arrow
        aws_ec2metadata
        aws_s3
        aws_signature
        config
        DT
        devtools
        flexdashboard
        geojsonsf
        ggforce
        jsonlite
        languageserver
        leaflet
        leaflet_extras
        leaflet_extras2
        lubridate
        INBOtheme
        plotly
        plyr
        reshape2
        reactable
        sf
        shinycssloaders
        shinyjs
        slickR
        tidyverse
        units
        webshot2
      ];
    
    in {
      devShells = {
        default = rUtils.lib.mkRShell {
          pkgs = pkgs;
          packages = [ 
            rpackages 
            pkgs.vscodium
            pkgs.awscli2
          ];
        };
      };
    }

  );
}