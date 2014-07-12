#!/bin/bash
apt-get install opam
apt-get install pkg-config
apt-get install m4
apt-get install libpcre3-dev
apt-get install libssl-dev
apt-get install libsqlite3-dev
apt-get install camlp4-extra

opam install pgocaml
opam install text

opam install pprint
opam install cmdliner

git clone https://github.com/klakplok/goji.git
cd goji
make
make install
cd ..

git clone https://github.com/yetanotherion/fbgraph_ml.git
cd fbgraph_ml
make
make install
cd ..

opam install macaque_lwt
opam install safepass
apt-get install libmagickcore-dev
opam install imagemagick

apt-get install postgresql
sudo -u postgres createuser --superuser $USER
