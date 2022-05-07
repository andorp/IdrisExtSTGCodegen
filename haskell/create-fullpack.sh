#!/bin/sh -e

stack --stack-root `pwd`/.stack-root build
mkfullpak -a ./.stack-work/dist/x86_64-linux/Cabal-3.2.0.0/build/idris-haskell-interface/idris-haskell-interface.o_ghc_stgapp
rm ../data/idris-haskell-interface.fullpak
cp ./.stack-work/dist/x86_64-linux/Cabal-3.2.0.0/build/idris-haskell-interface/idris-haskell-interface.fullpak ../data/
