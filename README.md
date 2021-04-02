# sonny76
## dev env
nix-env -iA nixos.ghc\
nix-env -iA nixos.cabal-install\
nix-env -iA nixos.stack\
nix-env -iA nixos.nodejs\
npm install sass

nix-env -iA nixos.atom\
nix-env -iA nixos.haskellPackages.haskell-language-server\
nix-env -iA nixos.ghcid\
apm install language-haskell atom-ide-ui haskell

### optional
apm install fonts autosave-onchange linter-hlint atom-beautify\
apm install last-cursor-position language-markdown sass\
nix-env -iA nixos.hlint\
cabal install hindent\
*add cabal bin to PATH*

## hack
./watch-sass\
./build\
./run\
firefox localhost:8080

## release (takes ~2h at first run)
nix-build

## deploy
nix-build && ./deploy
