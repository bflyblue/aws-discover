{
  description = "aws-discover";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    # until 2.0 is released
    amazonka = {
      url = "/home/shaun/devel/3rdparty/amazonka";
      flake = false;
    };

  };

  outputs = { self, nixpkgs, flake-utils, amazonka, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hlib = pkgs.haskell.lib;
        amazonkaLib = name:
          haskellPackages.callCabal2nix name "${amazonka}/lib/${name}";
        amazonkaService = name:
          haskellPackages.callCabal2nix name "${amazonka}/lib/services/${name}";
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            amazonka = amazonkaLib "amazonka" { };
            amazonka-core = hlib.dontCheck (amazonkaLib "amazonka-core" { });
            amazonka-test = amazonkaLib "amazonka-test" { };
            amazonka-sso = amazonkaService "amazonka-sso" { };
            amazonka-sts = amazonkaService "amazonka-sts" { };

            amazonka-apigateway = amazonkaService "amazonka-apigateway" { };
            amazonka-apigatewayv2 = amazonkaService "amazonka-apigatewayv2" { };
            amazonka-cloudwatch-logs =
              amazonkaService "amazonka-cloudwatch-logs" { };
            amazonka-ec2 = amazonkaService "amazonka-ec2" { };
            amazonka-lambda = amazonkaService "amazonka-lambda" { };
            amazonka-rds = amazonkaService "amazonka-rds" { };
            amazonka-resourcegroupstagging =
              amazonkaService "amazonka-resourcegroupstagging" { };
            amazonka-s3 = amazonkaService "amazonka-s3" { };
            amazonka-secretsmanager =
              amazonkaService "amazonka-secretsmanager" { };
            amazonka-xray = amazonkaService "amazonka-xray" { };
          };

        };
        hls = pkgs.haskell-language-server;
        packageName = "aws-discover";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self { };

        packages.default = self.packages.${system}.${packageName};

        devShells.default = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.${packageName} ];
          buildInputs = [ haskellPackages.cabal-install hls ];
          withHoogle = true;
        };
      });
}
