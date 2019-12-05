#!/usr/bin/bash

echo "You need stack installed using whatever package manager you like best"
echo "If on arch, I would recommend Stack static on the AUR"

sudo cp ./xest.desktop /usr/share/xsessions/xest.desktop
sudo cp ./startxest /usr/local/bin/

mkdir -p ~/.config/xest/
cp ./config/* ~/.config/xest/

echo "Building the project now"
echo "If this is your first time, it will take a while"
echo "Stack first builds all dependencies then builds Xest"
echo "Thankfully, Stack caches things so future builds will be MUCH faster"
stack build && stack install
sudo cp ~/.local/bin/neXtWM-exe /usr/local/bin/neXtWM-exe

echo "If everything worked correctly, Xest should be an option in your display manager"
echo "However, ou might have to restart it or reboot before you see Xest"

echo "You also probably want to check out the ~/.config/xest/startup.sh file"
echo "It contains my own startup script which makes plenty of assumptions about the other tools on your computer"
echo "Replace it with whatever you whould normally put in your i3 exec's"
