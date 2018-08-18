# neXtWM / Xest Window Manager

## How to Test

Note that these commands are in the `fish terminal`.

Use `Xephyr -br -ac -noreset -screen 800x600 :99` to start up a little X server on Display :99.

Now start the window manager with DISPLAY:=99.

This can be done easily in fish with
`env DISPLAY=:99 stack exec neXtWM-exe`.
