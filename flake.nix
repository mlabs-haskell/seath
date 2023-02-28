{
  inputs = {
    plutip.url = github:mlabs-haskell/plutip/8364c43ac6bc9ea140412af9a23c691adf67a18b;
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/v5.0.0;
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, cardano-transaction-lib, plutip, ... }:
    let
      # COMMON
      supportedSystems = [ "x86_64-linux" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          haskell-nix.overlay
          cardano-transaction-lib.overlays.purescript
          cardano-transaction-lib.overlays.runtime
        ];
      #TODO : this triggers a core dump but may be needed for plutip?
      #  inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          nativeBuildInputs = [
            pkgs'.fd
            pkgs'.git
            pkgs'.nixpkgs-fmt
            pkgs.easy-ps.purs-tidy
            pkgs'.haskell.packages.${on-chain.ghcVersion}.cabal-fmt
            pkgs'.haskell.packages.${on-chain.ghcVersion}.fourmolu
          ];
          inherit (pkgs'.lib) concatStringsSep;
          otherBuildInputs = [ pkgs'.bash pkgs'.coreutils pkgs'.findutils pkgs'.gnumake pkgs'.nix ];
          format = pkgs.writeScript "format"
            ''
              export PATH=${concatStringsSep ":" (map (b: "${b}/bin") (otherBuildInputs ++ nativeBuildInputs))}
              export FOURMOLU_EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor"
              set -x
              purs-tidy format-in-place $(fd -epurs)
              fourmolu $FOURMOLU_EXTENSIONS --mode inplace --check-idempotence $(find on-chain/{exporter,src} -iregex ".*.hs")
              nixpkgs-fmt $(fd -enix)
              cabal-fmt --inplace $(fd -ecabal)
            '';
        in
        {
          inherit format;
        }
      ;

      # ON-CHAIN: PlutusTx, Cardano.Api

      on-chain = rec {
        ghcVersion = "ghc8107";

        inherit (plutip.inputs) nixpkgs haskell-nix;

        nixpkgsFor = system: import nixpkgs {
          inherit system;
          overlays = [
            haskell-nix.overlay
            (import "${plutip.inputs.iohk-nix}/overlays/crypto")
          ];
          inherit (haskell-nix) config;
        };
        nixpkgsFor' = system: import nixpkgs { inherit system; inherit (haskell-nix) config; };

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgsFor' system;
          in
          pkgs.haskell-nix.cabalProject {
            src = ./on-chain;
            compiler-nix-name = ghcVersion;
            index-state = "2022-05-25T00:00:00Z";
            cabalProject = ''
              packages: ./.
            '';
            inherit (plutip) cabalProjectLocal;
            extraSources = plutip.extraSources ++ [
              {
                src = "${plutip}";
                subdirs = [ "." ];
              }
            ];
            modules = plutip.haskellModules;
            shell = {
              withHoogle = true;
              exactDeps = true;
              nativeBuildInputs = with pkgs'; [
                git
                haskellPackages.apply-refact
                fd
                cabal-install
                hlint
                haskellPackages.cabal-fmt
                haskellPackages.fourmolu
                nixpkgs-fmt
              ];
              tools.haskell-language-server = { };
              additional = ps:
                with ps; [
                  cardano-api
                  plutus-ledger
                  plutus-ledger-api
                  plutus-script-utils
                  plutus-tx
                  plutus-tx-plugin
                  serialise
                ];
            };
          };

        script-export = system:
          let
            pkgs' = nixpkgsFor' system;
            exporter = ((projectFor system).flake { }).packages."seath:exe:script-export";
          in
          pkgs'.runCommandLocal "script-export" { }
            ''
              ln -s ${exporter}/bin/script-export $out
            '';

        exported-scripts = system:
          let
            pkgs' = nixpkgsFor' system;
            exporter = ((projectFor system).flake { }).packages."seath:exe:script-export";
          in
          pkgs'.runCommand "exported-scripts" { }
            ''
              set -e
              mkdir $out
              ${exporter}/bin/script-export
            '';
      };

      # OFF-CHAIN: CTL, Plutip

      off-chain = {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            exporter = ((on-chain.projectFor system).flake { }).packages."seath:exe:script-export";
          in
          pkgs.purescriptProject {
            inherit pkgs;
            projectName = "seath-off-chain";
            strictComp = false; # TODO: this should be eventually removed
            src = pkgs.runCommandLocal "generated-source" { }
              ''
                set -e
                cp -r ${./off-chain} $out
                chmod -R +w $out
                ${exporter}/bin/script-export $out/src
              '';
            packageJson = ./off-chain/package.json;
            packageLock = ./off-chain/package-lock.json;
            shell = {
              packageLockOnly = true;
              packages = with pkgs; [
                bashInteractive
                docker
                fd
                nodePackages.eslint
                nodePackages.prettier
                ogmios
                plutip-server
                easy-ps.purs-tidy
              ];
              shellHook =
                ''
                  export LC_CTYPE=C.UTF-8
                  export LC_ALL=C.UTF-8
                  export LANG=C.UTF-8
                '';
            };
          };
      };
    in
    {
      inherit nixpkgsFor;

      on-chain = {
        project = perSystem on-chain.projectFor;
        flake = perSystem (system: (on-chain.projectFor system).flake { });
      };

      off-chain = {
        project = perSystem off-chain.projectFor;
        flake = perSystem (system: (off-chain.projectFor system).flake { });
      };

      packages = perSystem (system:
        self.on-chain.flake.${system}.packages
        // {
          script-export = on-chain.script-export system;
          exported-scripts = on-chain.exported-scripts system;
        }
      );
      checks = perSystem (system:
        self.on-chain.flake.${system}.checks
        // {
          plutip-tests = self.off-chain.project.${system}.runPlutipTest { testMain = "PlutipCanary"; };
        }
      );

      devShells = perSystem (system: {
        on-chain = self.on-chain.flake.${system}.devShell;
        off-chain = self.off-chain.project.${system}.devShell;
      });

      apps = perSystem (system: {
        default-ctl-runtime = (nixpkgsFor system).launchCtlRuntime { };
        docs = self.off-chain.project.${system}.launchSearchablePursDocs { };
        ctl-docs = cardano-transaction-lib.apps.${system}.docs;
        script-export = {
          # nix run .#script-export -- off-chain/src
          type = "app";
          program = (on-chain.script-export system).outPath;
        };
        # FIXME: fix formatting and add check to CI
        # https://github.com/mlabs-haskell/seath/issues/7
      
        # format = {
        #   type = "app";
        #   program = (formatCheckFor system).format.outPath;
        # };
      });
    };
}
