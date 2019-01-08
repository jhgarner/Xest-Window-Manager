# Xest Window Manager

## What is it?

Xest in a window manager for Xorg inspired by projects like XMonad or i3. Xest is extremely incomplete in it's current state and not suitable for actual use.

Xest aims to improve upon other window managers by allowing the arbitrary nesting of different Tilers. A Tiler is a way of organizing windows. For example, a Tiler might organize windows horizontally, vertically, or as workspaces.

Xest also features Vim-like modes. In the default config, pressing Alt brings you to Normal mode and pressing Escape brings you to Insert mode. Sometimes though, you only want to enter a couple of commands in the new mode. To make that easier, you can hold down the mode change key to temporarily enter the new mode. Temporarily entering a mode is extremely similar to how pretty much every program except Vim works.

## How does it work?

Xest is based on parsing a stream of Actions. Each Action represents either an event, like a key being pressed, or a command, like changing the layout. When executing an Action, Xest may return additional Actions to be appended onto the end of the stream. For example, a key event might returns actions bound to that key press. In the config file, a subset of Actions can be bound to keys or mode changes.

Xest also features a Tiler called the Controller Input which moves in and out of the Tiler tree keeping track of nesting.

## How to Test

Note that these commands are in the `fish terminal`.

Use `Xephyr -br -ac -noreset -screen 800x600 :99` to start up a little X server on Display :99.

Now start the window manager with DISPLAY:=99.

This can be done easily in fish with
`env DISPLAY=:99 stack exec neXtWM-exe`.
or in other shells with
`export DISPLAY=:99; stack exec neXtWM-exe`.
