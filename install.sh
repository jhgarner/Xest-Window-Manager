#!/usr/bin/bash

export PATH="$coreutils/bin"

>&2 echo $bin

# mkdir -p $out/share/xsessions/
# cp $desktop $out/share/xsessions/xest.desktop

mkdir -p $out/bin/
cp $bin $out/bin/xest

# mkdir -p $out/etc/xest/
# cp $config/* $out/etc/xest/

# exit 1
