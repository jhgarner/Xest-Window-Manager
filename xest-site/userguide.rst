---
title: User guide
date: 2015-1-1
---

Post Install
============

Xest will crash if it can't find a config file in your home directory. If you
installed the Arch package, you should be able to copy the contents of the "/etc/xest/config/
folder into "~/.config/xest/config/". Once you do that, you will need to edit
the config files. "config.dhall" is kind of messy right now, but it should have
most of what you need. Just make sure to change any hard-coded strings for your
system. Most likely you'll need to change the terminal from "kitty/termite" to
something else. You also probably need to change the startup file and font
locations. The "startup.sh" will also need some changes. Add whatever you want
to run on boot and remove the things that you don't have. Use "&" to make sure
it doesn't get hung up on any lines that spawn long running commands.

Using Xest
=============

The following key bindings assumes you're using the default config.dhall (after
performing the post install instructions).

Modes and Keys
--------------

Once you've launched Xest, you should see your desktop wallpaper and not much
else. Begin by pressing the Super/Windows key. A pink border should have
appeared around your desktop. This border signifies that you are in normal mode.
The color and position of the border tell you where you are in Xest's tree. You
can also look at the text on the border to get an idea for where you are. The
"@" sign tells you where the monitor is and the "*" tells you where the input
controller is. Those will be explained more below.

Pressing "t" should launch your terminal while "d" should launch your launcher.

At some point, you probably want to leave normal mode. You can do that using the
escape key. Pressing 3 keys (windows, d, escape) every time you want to launch a program seems kind
of long. Although this mode based system has it's advantages on longer commands,
it tends to be less efficient when you just want to perform one action.

Thankfully, Xest has a solution. You can hold down any key that changes your
mode to temporarily switch into it. As soon as you release that key, you go back
to the mode you were in before. How does Xest identify between temporarily
entering a mode and permanently entering one? If you press another bound key
while holding down the one that changed your mode, Xest will assume you want it
to be temporary. If you let go of the mode key before pressing another, Xest
assumes you want to remain in the new mode.

Basically, this is an extremely long winded way to say you can press "Super+d"
like you would in i3 to launch rofi/dmenu or "Super+t" to launch the terminal.
Any key press in Xest supports temporary and permanent modes.

One of the cool implications of this is the NormalS mode in the default config.
In Xest, binding the "I" key is the exact same as binding the "i" key. This
might seem like a problem, until you realize that Shift is just another key that
can be bound. The NormalS mode is triggered when you press the Shift key while
in Normal mode. Because of how temporary modes work, this essentially emulates
being able to bind both "I" and "i".

If NormalS is just a mode, what happens if I press and
release the Shift key without hitting anything else? In that case, you enter
NormalS mode permanently and can hit a string of capital letters with ease. You
can move back to Normal mode by pressing escape. To get back into Insert mode,
hit escape again. None of this is hard-coded into Xest. You can find all of this
logic in the config.dhall file.

Keys in Xest can be bound to things called actions. This will be covered more in
the configuring Xest section.

Tilers
------

As mentioned elsewhere, Xest represents windows as a tree. This means that Xest
has some kind of root element in the tree and a bunch of nodes coming off of it.

Nodes in Xest are called Tilers. A Tiler is simply something which can contain 0
or more children and might hold additional metadata. After most actions, Xest
prunes the tree and removes the empty Tilers. You can associate keybindings with
inserting Tilers into the tree.

If you're every confused about what the tree looks like, just enter Normal mode.
On the top most border, a textual representation of the tree is provided. The
text shows you the path from the root of the tree all the way to the currently
focused window.

Here are some of the Tilers:

Wrap
^^^^

A Wrap Tiler represents a leaf on the tree. It simple wraps a normal window and
has no children.

Many
^^^^^^^^^^

Many is a complicated Tiler and holds a lot of power. It's called Many because
it can hold many children. By default, each child is tiled horizontally like in
i3. If you want to have 1 background Tiler with the rest floating on top, you
can tell Many to change to a "floating" style. If you want a 2 column approach
like Xmonad uses, you can switch to the "twoCols" style. You can also rotate
everything using the "rotate" modifier. This turns the "horizontally" style into a
vertical one and the "twoCols" style into something that might be called
"twoRows". Finally, you can set the "full" modifier to make the currently focused
child take up all of the space.

How do you create grids of windows? The children to any Tiler are other Tilers.
If you nest Many multiple times, you can create arbitrarily complex layouts. If
you nest Many with the "full" modifier set, you can get something like nested
workspaces.

On the border, a Many Tiler is represented as
"\|<modifier>-<style>-<#children>-<focusedChild>\|".

Monitor
^^^^^^^

This Tiler tells Xest when to start rendering on a given screen. By default, the
Monitor is the root of the screen. You can zoom it inwards to minimize entire
branches of the tree. This is another way to create a form of workspaces.

On the border, a Monitor is represented as "@".

Input Controller
^^^^^^^^^^^^^^^^

This Tiler let's you control where you want Actions to be applied and where new
windows should be placed. When a new window is created, it is made a child of
whatever comes immediately after the Input Controller. Likewise, if you perform
some action, it is applied to whatever is the Input Controller's immediate
child. Like the Monitor, you can move the Input Controller around the screen.

On the border, an Input Controller is represented as "*".

Zooming
-------

Take a moment to imagine your typical Gnome/Windows workflow assuming you're
making use of virtual desktops. In those options, you have some kind of
task or overview mode to help you see the bigger picture. For example, in Gnome
it happens when you press the windows key. In Windows it happens when you press the virtual desktop button on the taskbar. When
you enter this kind of view, it's almost like you're "zooming out" from the
desktop you had been working in. Instead of looking at one desktop, you can now
see several. Likewise, you can usually click on one of the desktops to zoom back in on
whatever you were working on.

This zooming concept is taken to the extreme in Xest as it's your primary way of
moving around. By making zooming explicit, we can get features like workspaces
and window maximization in one "simple" idea. In no time, you'll find your efficiency
zooming past its former self :)

Zooming can be applied to either the Input Controller or the Monitor. Check out
the list of actions when configuring Xest for more details.

Popping/Pushing
---------------

Xest keeps a stack of popped tilers in it's memory.
When you pop a window, it gets removed from the tree. You can add it back into
the tree using the Push action.

Unlike other Window Managers, Xest doesn't have a dedicated Action for moving
windows to other desktops. Instead, you have to use popping
and pushing to accomplish that.

One of the benefits to this is Xest can yank entire sections of your tree at
once.

Configuring Xest
================

Dhall
-----

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

Config Sections
---------------
The Dhall config is a dictionary with a few sections.

StartupScript
^^^^^^^^^^^^^
A string containing a script to run in your shell. This is useful for starting
things like Compton, Feh, Polybar, etc.

InitialMode
^^^^^^^^^^^^

A single mode which will be used once you start up Xest.

fontLocation
^^^^^^^^^^^^

The location of the font to use on the border. This is probably something in
"/usr/share/fonts/...".

KeyBindings
^^^^^^^^^^^

A list of key bindings. See the example config file for more info on the format.
The weirdest field is the exitActions one. This field contains a list of actions
that will be performed when the key is released assuming it didn't trigger a
permanent mode change.

You can get most key names using the "xev" tool from your distro.


Actions
-------

Actions are performed when you press a key binding or change modes. You can
perform multiple actions at any time. If an action is invalid, it should do
nothing instead of crashing Xest.

Insert
^^^^^^

Adds a Many Tiler right after the Input Controller. Whatever used to be the Input
Controller's child will become the child of the Many Tiler.

RunCommand (s: Text)
^^^^^^^^^^^^^^^^^^^^

Runs a command using your shell.

ChangeModeTo (m: Mode)
^^^^^^^^^^^^^^^^^^^^^^

Changes the current mode. For example, I usually map the Windows key to Normal
mode and the escape key to Insert mode.

ShowWindow (w: Text)
^^^^^^^^^^^^^^^^^^^^

Shows a given window given it's class name. For example, this can be used to
hide or show a taskbar. You should probably avoid using this on windows managed
by Xest.

HideWindow (w: Text)
^^^^^^^^^^^^^^^^^^^^

The opposite of the above Action.

ZoomInInput/ZoomInMonitor
^^^^^^^^^^^^^^^^^^^^^^^^^

Zooms the input/monitor away from the root towards whatever window is focused.

ZoomOutInput/ZoomOutMonitor
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The inverse of the above Action.

PopTiler
^^^^^^^^

Yanks a Tiler onto the stack.

PushTiler
^^^^^^^^^

The inverse of the above Action.

MakeEmpty
^^^^^^^^^

If a Many Tiler is the direct child of the Input Controller, this will create a
new child for that Many Tiler and will place the Input Controller inside of it.
If the Many Tiler has the full modifier set, this could be seen as creating a new workspace.

MoveToFront
^^^^^^^^^^^

If a Many Tiler is the direct child of the Input Controller, this will move
whatever the currently focused child is to the "front" of the Tiler. What the
front means depends on the style and modifiers of the Tiler.

ChangeToHorizontal
^^^^^^^^^^^^^^^^^^

If a Many Tiler is the direct child of the Input Controller, this will change to
the horizontal style.

ChangeToFloating
^^^^^^^^^^^^^^^^

If a Many Tiler is the direct child of the Input Controller, this will change to
the floating style.

ChangeToTwoCols
^^^^^^^^^^^^^^^

If a Many Tiler is the direct child of the Input Controller, this will change to
the twoCols style.

setRotate
^^^^^^^^^

If a Many Tiler is the direct child of the Input Controller, this will change to
the rotate modifier.

setFull
^^^^^^^^^

If a Many Tiler is the direct child of the Input Controller, this will change to
the full screen modifier.

setNoMod
^^^^^^^^^

If a Many Tiler is the direct child of the Input Controller, this will remove
whatever the current modifier is.

ChangeNamed (n: Text)
^^^^^^^^^^^^^^^^^^^^^

Change to a different child. Currently, the only valid names are numbers.

Move (d: Direction)
^^^^^^^^^^^^^^

Changes children either forwards or backwards depending on the value
passed to it.

KillActive
^^^^^^^^^^

Kills the currently focused window.

ExitNow
^^^^^^^

Exits from Xest without trying to kill anything nicely first. If you trigger
this on accident, you will likely lose work.

ToggleLogging
^^^^^^^^^^^^^

Each call to this action toggles whether logging happens in the /tmp/xest.log
file. By default, it is off. Each time you turn it on, the file is overwritten.

ZoomMonitorToInput
^^^^^^^^^^^^^^^^^^

Moves the Monitor so that it is right behind the Input Controller.

ZoomInputToMonitor
^^^^^^^^^^^^^^^^^^

Moves the Input Controller so that it is right in front of the Monitor.
