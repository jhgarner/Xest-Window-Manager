---
title: User guide
date: 2015-1-1
---

# Walkthrough

This walkthrough will cover the basic aspects of Xest in a way that lets you
follow along. There will also be plenty of videos along the way.

## First Actions

If you installed the Arch package, Xest should have everything it needs to run.
Reboot and using your display manager, select Xest from the list that appears
and log in. If everything worked, you should see a blank screen. At this point,
you should press the windows/super key. Ignoring the wallpaper, compton, and
polybar that I've configured, your screen should look like the one below.

<img src="images/blank.png" width=/>



The pink border that appeared signifies that you've entered normal mode.
Previously, you were in insert mode. You can reenter insert mode by pressing
escape. 

Press and release the super key to enter normal mode, then press and release the
enter key. This should open up a terminal on your system.

<video playsinline autoplay muted loop>
  <source src=/images/launchterminal.mp4>
</video>

Notice the text on the border. The @ symbol represents the monitor's location.
Anything to the right of the monitor is rendered.
The * represents the input controller. The borders you see are drawn around
whatever is just to the right of the input controller. The word "Window" shows
that there is a window in the tree.

Typing out 3 keys to launch a terminal is kind of slow. Clicking and releasing
the Windows key triggers a "permanent" switch to normal mode. If you only want
to temporarily enter normal mode and launch a terminal: hold the windows key,
press enter, and release both. In other words, use the windows key the same way
you would when typing out a keyboard shortcut in almost any other program.

If you haven't already, open a second terminal. The text above should have
changed to "@*|H-2-2|Window". The new part is "|H-2-2|". The bars are just there
to tell you that everything in between them is a single "unit" or tiler. This
specific tiler is called the "Many" tiler. The "H" tells you that your windows
are tiled horizontally. The first "2" tells you how many windows are in the
tiler, and the second "2" tells you which of those is currently focused.

The Monitor, Input Controller, and Window are other tilers.

While in normal mode, try typing the numbers 1 and 2 on your keyboard. That
should have let you change the focused window. Focus will also follow the mouse.

<video playsinline autoplay muted loop>
  <source src=/images/switchterm.mp4>
</video>

## Modifiers

Once again in normal mode, press the m key. Whatever terminal was focused should
now be taking up the entire screen. The Many tiler should now have "f-"
prepended to it. The "f" stands for the fullscreen modifier. You can also press
"v" to trigger the vertical modifier and "c" to clear all modifiers. Also
notice how, no matter the modifier, the number keys still change the focus in
the way you would expect.

In addition to modifiers, you can also change the type of arrangement the Many
tiler will make. Pressing Shift then t switches to the two column arrangement.
Try adding more windows and see where they get added. Pressing Shift then f
switches to the floating arrangement. You can move those using the mouse in
normal mode. Finally, pressing Shift then n puts you back in the horizontal
arrangement. Try playing around with the left and right mouse buttons in the
different arrangements while in normal mode.

<video playsinline autoplay muted loop>
  <source src=/images/floating.mp4>
</video>

Technically, Xest doesn't actually have support for binding capital letters.
Instead, the shift key triggers a mode change. If you hold down shift then click
another bound key, you'll only enter the shift mode temporarily and it will act
like you bound a capital letter. If you just click the shift key on its own, you
can press escape to reenter normal mode and escape again to reenter insert mode.

## Zooming

With our Many tiler set with the fullscreen modifier, things act a lot like
workspaces in other environments. Launching new terminals places them in new
workspaces. If you want to launch a terminal in the current workspace, you'll
need to use zooming. Instead of having an example follow an explanation, I'll
start with the example:

<video playsinline autoplay muted loop>
  <source src=/images/zooming.mp4>
</video>

Xest is based on the idea of nested tilers. To navigate that nesting, you use
the "i" and "o" keys to zoom in and out respectively. Zooming in means moving
the input controller one tiler to the right. Zooming out moves it to the left.
The border colors change based on the current depth of the input controller.
Since new windows get added to whatever comes to the right of the input
controller, zooming in let us add the new terminal to the current workspace. If
we want to jump to the other workspace, we need to zoom back out.

You may be wondering what would happen if you added the fullscreen modifier to
the inner Many tiler. Well, that gets you nested workspaces!

## Cut/Paste
The last feature we'll cover here is moving tilers from one location to another.
From normal mode, click the shift key. Next, click the d key to
cut the tiler to the right of the input controller and place it in the cut
stack. Press p to paste whatever is on top of the stack in front of the input
controller. Here is how we can combine all these features to move windows:

<video playsinline autoplay muted loop>
  <source src=/images/paste.mp4>
</video>

There's a lot going on here. Try recreating the video on your own machine.
Getting an intuition for how the tree works takes a little time but it's the key
to using Xest efficiently.

In the video, you'll notice I copied both single windows and entire groups. That
works because Xest only operates on tilers, not windows. It just so happens that
Xest automatically wraps every window in its own tiler.

## Moving On
That's all for the walkthrough. At this point, you probably want to check out
the config file and some of the other keybindings. The default config is in
"/etc/xest/config.dhall". You can copy and modify that file in
"~/.config/xest/config.dhall". In addition, the startup scripts should be
changed to launch whatever programs you want to run when you log in.

If you want to launch more than a terminal, press space in normal mode. That
should launch rofi or dmenu depending on what you have installed.



# Docs
The following is a more technical and complete description of what Xest can do.

## Tilers

### Wrap

A Wrap Tiler represents a leaf on the tree. It simple wraps a normal window and
has no children.

### Many

Many is a complicated Tiler and holds a lot of power. It's called Many because
it can hold many children. By default, each child is tiled horizontally like in
i3. If you want to have 1 background Tiler with the rest floating on top, you
can tell Many to change to a "floating" style. If you want a 2 column approach
like Xmonad uses, you can switch to the "twoCols" style. You can also rotate
everything using the "rotate" modifier. This turns the "horizontally" style into
a vertical one and the "twoCols" style into something that might be called
"twoRows". Finally, you can set the "full" modifier to make the currently
focused child take up all of the space.

How do you create grids of windows? The children to any Tiler are other Tilers.
If you nest Many multiple times, you can create arbitrarily complex layouts. If
you nest Many with the "full" modifier set, you can get something like nested
workspaces.

On the border, a Many Tiler is represented as

``|<modifier>-<style>-<#children>-<focusedChild>|``.

### Monitor

This Tiler tells Xest when to start rendering on a given screen. By default, the
Monitor is the root of the screen. You can zoom it inwards to minimize entire
branches of the tree. This is another way to create a form of workspaces.

On the border, a Monitor is represented as "@".

### Input Controller

This Tiler let's you control where you want Actions to be applied and where new
windows should be placed. When a new window is created, it is made a child of
whatever comes immediately after the Input Controller. Likewise, if you perform
some action, it is applied to whatever is the Input Controller's immediate
child. Like the Monitor, you can move the Input Controller around the screen.

On the border, an Input Controller is represented as "*".

## Configuring Xest

### Dhall

Xest is configured using a language called Dhall. I would highly recommend
reading through their website and looking at the interactive examples.

In essence, Dhall is a functional configuration language. It has powerful
features like first class functions and polymorphism. Dhall also prides itself
in being a total language; you can be certain that a Dhall program will not
infinitely loop or run any IO beyond importing other Dhall files.

That import feature is one of Dhall's greatest strengths and makes it really
easy to share your configs. Dhall supports loading config files from local and
online sources. After verifying that the configs you want are indeed good, you
can run Dhall freeze to cache them locally. This provides an alternative to
copying and pasting configs from Github.

### Config Sections

The Dhall config is a dictionary with a few sections.

#### StartupScript

A string containing a script to run in your shell. This is useful for starting
things like Compton, Feh, Polybar, etc.

#### InitialMode

A single mode which will be used once you start up Xest.

##### fontLocation

The location of the font to use on the border. This is probably something in
"/usr/share/fonts/...".

#### KeyBindings

A list of key bindings. See the example config file for more info on the format.
The weirdest field is the exitActions one. This field contains a list of actions
that will be performed when the key is released assuming it didn't trigger a
permanent mode change.

You can get most key names using the "xev" tool from your distro.


## Actions

Actions are performed when you press a key binding or change modes. You can
perform multiple actions at any time. If an action is invalid, it should do
nothing instead of crashing Xest.

### Insert

Adds a Many Tiler right after the Input Controller. Whatever used to be the Input
Controller's child will become the child of the Many Tiler.

### RunCommand (s: Text)

Runs a command using your shell.

### ChangeModeTo (m: Mode)

Changes the current mode. For example, I usually map the Windows key to Normal
mode and the escape key to Insert mode.

### ShowWindow (w: Text)

Shows a given window given it's class name. For example, this can be used to
hide or show a taskbar. You should probably avoid using this on windows managed
by Xest.

### HideWindow (w: Text)

The opposite of the above Action.

### ZoomInInput/ZoomInMonitor

Zooms the input/monitor away from the root towards whatever window is focused.

### ZoomOutInput/ZoomOutMonitor

The inverse of the above Action.

### PopTiler

Yanks a Tiler onto the stack.

### PushTiler

The inverse of the above Action.

### MakeEmpty

If a Many Tiler is the direct child of the Input Controller, this will create a
new child for that Many Tiler and will place the Input Controller inside of it.
If the Many Tiler has the full modifier set, this could be seen as creating a new workspace.

### MoveToLoc (n: Nat)

If a Many Tiler is the direct child of the Input Controller, this will move
whatever the currently focused child is to the n'th index of the Tiler. N here
is 1 indexed to better match a user's keyboard.

### ChangeToHorizontal

If a Many Tiler is the direct child of the Input Controller, this will change to
the horizontal style.

### ChangeToFloating

If a Many Tiler is the direct child of the Input Controller, this will change to
the floating style.

### ChangeToTwoCols

If a Many Tiler is the direct child of the Input Controller, this will change to
the twoCols style.

### setRotate

If a Many Tiler is the direct child of the Input Controller, this will change to
the rotate modifier.

### setFull

If a Many Tiler is the direct child of the Input Controller, this will change to
the full screen modifier.

### setNoMod

If a Many Tiler is the direct child of the Input Controller, this will remove
whatever the current modifier is.

### ChangeNamed (n: Text)

Change to a different child. Currently, the only valid names are numbers.

### Move (d: Direction)

Changes children either forwards or backwards depending on the value
passed to it.

### KillActive

Kills the currently focused window.

### ExitNow

Exits from Xest without trying to kill anything nicely first. If you trigger
this on accident, you will likely lose work.

### ToggleLogging

Each call to this action toggles whether logging happens in the /tmp/xest.log
file. By default, it is off. Each time you turn it on, the file is overwritten.

### ZoomMonitorToInput

Moves the Monitor so that it is right behind the Input Controller.

### ZoomInputToMonitor

Moves the Input Controller so that it is right in front of the Monitor.

### ToggleDocks

Toggles whether to show any docks such as Polybar. Docks are hidden when you
enter full screen although they won't necessarily be restored automatically so
you might want to have this as a keybinding somewhere.
