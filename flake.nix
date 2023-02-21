{
  inputs = {
    plutip.url = github:mlabs-haskell/plutip/8364c43ac6bc9ea140412af9a23c691adf67a18b;
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/fcdd234cfe71345990f09eb1d6b4e2274faa2405;
    haskell-nix.follows = "plutip/haskell-nix";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutip, cardano-transaction-lib, ... }:
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
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          nativeBuildInputs = with pkgs'; [
            pkgs.asy-ps.purs-tidy
            fd
            git
            nixpkgs-fmt
            haskell.packages.${on-chain.ghcVersion}.cabal-fmt
            haskell.packages.${on-chain.ghcVersion}.fourmolu
          ];
          inherit (pkgs'.lib) concatStringsSep;
          otherBuildInputs = [ pkgs'.bash pkgs'.coreutils pkgs'.findutils pkgs'.gnumake pkgs'.nix ];
          format = pkgs.writeScript "format"
            ''
              export PATH=${concatStringsSep ":" (map (b: "${b}/bin") (otherBuildInputs ++ nativeBuildInputs))}
              export FOURMOLU_EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor"
              set -x
              purs-tidy format-in-place $(fd -epurs)
              fourmolu $FOURMOLU_EXTENSIONS --mode inplace --check-idempotence $(find on-chain/{script-export,src} -iregex ".*.hs")
              nixpkgs-fmt $(fd -enix)
              cabal-fmt --inplace $(fd -ecabal)
            '';
        in
        {
          inherit format;
        }
      ;

      # ON-CHAIN part: plutus and plutus-apps

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
            script-export = ((projectFor system).flake { }).packages."seath:exe:script-export";
          in
          pkgs'.runCommandLocal "script-export" { }
            ''
              ln -s ${script-export}/bin/script-export $out
            '';

        exported-scripts = system:
          let
            pkgs' = nixpkgsFor' system;
            script-export = ((projectFor system).flake { }).packages."seath:exe:script-export";
          in
          pkgs'.runCommand "exported-scripts" { }
            ''
              set -e
              mkdir $out
              ${script-export}/bin/script-export
            '';
      };

      # OFF-CHAIN part: CTL, Testnet, etc.

      off-chain = {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            script-export = ((on-chain.projectFor system).flake { }).packages."seath:exe:script-export";
          in
          pkgs.purescriptProject {
            inherit pkgs;
            projectName = "seath";
            strictComp = false; # TODO: this should be eventually removed
            src = pkgs.runCommandLocal "generated-source" { }
              ''
                set -e
                cp -r ${./off-chain} $out
                chmod -R +w $out
                ${script-export}/bin/script-export $out/src
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
                ogmios-datum-cache
                plutip-server
                postgresql
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
        # FIXME: lines below break `nix flake check`
        # // {
        #   seath = self.off-chain.project.${system}.runPlutipTest { testMain = "Test"; };
        # }
      );

      devShells = perSystem (system: {
        on-chain = self.on-chain.flake.${system}.devShell;
        off-chain = self.off-chain.project.${system}.devShell;
      });

      apps = perSystem (system: {
        docs = self.off-chain.project.${system}.launchSearchablePursDocs { };
        ctl-docs = cardano-transaction-lib.apps.${system}.docs;
        script-export = {
          # nix run .#script-export -- off-chain/src
          type = "app";
          program = (on-chain.script-export system).outPath;
        };
        format = {
          type = "app";
          program = (formatCheckFor system).format.outPath;
        };
      });
    };
}
