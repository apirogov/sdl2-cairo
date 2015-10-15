#!/bin/sh
set -e
if [ ! -d "$HOME/libsdl2/lib" ]; then
  hg clone https://hg.libsdl.org/SDL SDL
  pushd .
  cd SDL
  mkdir build
  cd build
  ../configure --prefix=$HOME/libsdl2
  make
  make install
  popd
else
  echo "Using cached directory."
fi
