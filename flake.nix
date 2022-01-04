{
  description = "Simple pool implementation";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }: {
    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides =
          final.lib.composeExtensions
            (prev.haskell.packageOverrides or (_: _: { }))
            (hfinal: hprev: {
              psql =
                hfinal.callCabal2nix "psql"
                  (
                    final.nix-gitignore.gitignoreSourcePure
                      [
                        ./.gitignore
                        "*.nix"
                        "flake.lock"
                        "*.yaml"
                        "cabal.project*"
                        ".vscode"
                        ".github"
                        "*.md"
                      ]
                      self
                  )
                  { };
            });
      };
    };
  } // flake-utils.lib.eachDefaultSystem (system:
    with (import nixpkgs { inherit system; });
    {
      devShell = mkShell {
        buildInputs = [
          postgresql
          ghc
          cabal-install
          haskell-language-server
          hlint
          stylish-haskell
          nixpkgs-fmt
        ];
      };
    }
  );
}
