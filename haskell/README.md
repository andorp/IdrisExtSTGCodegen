Build project with:
stack --stack-root `pwd`/.stack-root build

To build fullpak:
mkfullpak -a ./.stack-work/dist/x86_64-linux/Cabal-3.2.0.0/build/idris-haskell-interface/idris-haskell-interface.o_ghc_stgapp

Copy file to data folder:
cp ./.stack-work/dist/x86_64-linux/Cabal-3.2.0.0/build/idris-haskell-interface/idris-haskell-interface.fullpak ../data/
