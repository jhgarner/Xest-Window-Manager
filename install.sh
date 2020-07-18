#!/usr/bin/env bash

# git clone "https://github.com/jhgarner/Xest-Window-Manager"
# cd Xest-Window-Manager

stack build
stack install --local-bin-path $out/bin/

desktop="
[Desktop Entry]
Name=Xest
Comment=The zesty window manager
Exec=/usr/bin/xest-exe
TryExec=/usr/bin/env
Type=Application
DesktopName=Xest
"

mkdir -p $out/share/xsessions/
echo $desktop > $out/share/xsessions/xest.desktop

mkdir -p $out/etc/xest/
cp config/* $out/etc/xest/
