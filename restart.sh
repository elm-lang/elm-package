#!/bin/bash

SERVE="serve-public-library"

pkill $SERVE
git pull && \
  ghc --make -O2 -threaded -hidir ghc_output -odir ghc_output Registry/Server.hs -o $SERVE && \
  ./$SERVE &