---
title: User guide
date: 2015-1-1
---

Using Xest
=============

The following assumes you're using the default config.dhall and have fixed up
the startup.sh file to your liking.

Modes and Keys
--------------

Once you've launched Xest, you should see your desktop wallpaper and not much
else. Begin by pressing the Super/Windows key. A pink border should have
appeared around your desktop. This signifies that you are in normal mode.

If you have Termite installed, pressing t while in normal mode will launch that
program. Likewise, pressing d while in normal mode brings up Rofi. If you don't
have either of those installed, you should edit the config.dhall file and
replace those names with something else.

At some point, you probably want to leave normal mode. You can do that using the
escape key. Pressing 3 keys every time you want to launch a program seems kind
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
like you would in i3 to launch rofi/dmenu. Any key press in Xest supports
temporary and permanent modes.

One of the cool extensions to this is the NormalS mode in the default config.
In Xest, binding the "I" key is the exact same as binding the "i" key. This
might seem like a problem, until you realize that Shift is just another key that
can be bound. The NormalS mode is triggered when you press the Shift key while
in Normal mode. Because of how temporary modes work, this essentially emulates
being able to bind both "I" and "i".

If you've already got a solid grasp on how Xest's modes work, you might be
confused by something. If NormalS is just a mode, what happens if I press and
release the Shift key without hitting anything else. In that case, you enter
NormalS mode permanently and can hit a string of capital letters with ease. You
can move back to Normal mode by pressing escape. To get back into Insert mode,
hit escape again.

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
focused leaf.

Here are some of the Tilers:

Wrap
^^^^

A Wrap Tiler represents a leaf on the tree. It simple wraps a normal window and
has no children.

Horizontal
^^^^^^^^^^

A Horizontal Tiler has a list of children and shows them to you horizontally.
Although they take up equal amounts of space by default, you can use the Resize
mode to adjust that with the mouse. A Horizontal Tiler is automatically created
if you create a new window while focusing on a Wrap.

Rotate
^^^^^^

A rotate Tiler tells it's children that the X and Y axes are swapped. Rotate
only takes a single child, but the change affects all of the children of that
child as well. If you put a Horizontal Tiler as the child of a Rotate Tiler, you
will create what looks like a Vertical Tiler. Note that Vertical Tilers don't
actually exist and don't have any special code. They are simply the result of
composing two Tilers together.

Full Screen
^^^^^^^^^^

A Full Screen Tiler takes the focused element of it's one child and renders that
without any of the unfocused children. If you put a Horizontal Tiler as the
child of a Full Screen Tiler, you get one way of emulating workspaces. If you
put a Monitor or Input Controller as the child of a Full Screen Tiler, the Full
Screen Tiler is essentially ignored while rendering.


Monitor
^^^^^^^

This Tiler tells Xest when to start rendering on a given screen. By default, the
Monitor is the root of the screen. You can zoom it inwards to minimize entire
branches of the tree.

Input Controller
^^^^^^^^^^^^^^^^

This Tiler let's you control where you want Actions to be applied and where new
windows should be placed. When a new window is created, it is made a child of
whatever comes immediately after the Input Controller. Likewise, if you perform
some action, it is applied to whatever is the Input Controller's immediate
child. Like the Monitor, you can move the Input Controller around the screen.

Floating
^^^^^^^^

In it's current state, this Tiler is kind of broken. When it worked better, it
let you escape the confines of a Tiling Window Manager by having floating Tilers
and a background Tiler.


Zooming
-------

Take a moment to imagine your typical Gnome/I3/Window workflow assuming you're
making use of virtual desktops. In most of those options, you have some kind of
task or overview mode to help you see the bigger picture. For example, in Gnome
it looks like this:...  and in Windows it looks something like this. Pay
particular attention to the little boxes showing you each virtual desktop. When
you enter this kind of view, it's almost like you're "zooming out" from the
desktop you had been working in. Instead of looking at one desktop, you can now
see several. Likewise, you can click on one of those desktops to "zoom in" on
whatever you're working on.

This zooming concept is taken to the extreme in Xest as it's your primary way of
moving around.  By making zooming explicit, we can get features like workspaces
and maximization in one "simple" idea. In no time, you'll find your efficiency
zooming past it's former self :)

Zooming can be applied to either the Input Controller or the Monitor. Check out
the list of actions when configuring Xest for more details.

Popping/Pushing
---------------

Xest keeps a stack of popped tilers in it's memory.
when you pop a window, it gets removed from the tree. You can add it back into
the tree using the Push action, its inverse.

Unlike other Window Managers, Xest doesn't have a dedicated Action for moving
windows to other desktops or swapping orders. Instead, you have to use popping
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

KeyBindings
^^^^^^^^^^^

A list of key bindings. See the example config file for more info on the format.

DefinedModes
^^^^^^^^^^^^

A list of defined modes. Intro Actions and exit actions are performed no matter
how you trigger entering the mode. The hasButtons field is used to control
whether a mode can be used to resize windows. If this is true, you won't be able
to interact with windows while you're in this mode. If hasBorders is true, the
colorful borders will be shown.

Actions
-------

Actions are performed when you press a key binding or change modes. You can
perform multiple actions at any time. If an action is invalid, it should do
nothing instead of crashing Xest.

Insert (t: Tilers)
^^^^^^^^^^^^^^^^^^

Adds the Tiler t right after the Input Controller. Whatever used to be the Input
Controller's child will become the child of t.

RunCommand (s: Text)
^^^^^^^^^^^^^^^^^^^^

Runs a command using your shell.

ChangeModeTo (m: Text)
^^^^^^^^^^^^^^^^^^^^^^

Changes to a mode in the definedModes section of the config. The parameter is
the mode's name. An invalid name will crash Xest.

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

MakeSpecial
^^^^^^^^^^^

Performs some special action depending on whatever comes after the Input
Controller. On a Floating Tiler, it makes the currently focused Tiler the
background. On a Horizontal Tiler, it adds a new, empty child.

ChangeNamed (n: Text)
^^^^^^^^^^^^^^^^^^^^^

Change to a different child. Currently, this is only supported for Horizontal
and Floating and the only valid names are numbers.

Move (d: Bool)
^^^^^^^^^^^^^^

Changes children either forwards or backwards depending on the Boolean value
passed to it. Only used by Horizontal and Floating.

KillActive
^^^^^^^^^^

Kills the currently focused window.

Exit
^^^^

Exits from Xest without trying to kill anything first. If you trigger this on
accident, you will likely lose work.

ToggleLogging
^^^^^^^^^^^^^

Each call to this action toggles whether logging happens in the /tmp/xest.txt
file. By default, it is off. Each time you turn it on, the file is overwritten.

ZoomMonitorToInput
^^^^^^^^^^^^^^^^^^

Moves the Monitor so that it is right behind the Input Controller.

ZoomInInputSkip/ZoomOutInputSkip
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Smart zoomers that try to jump over useless things like Rotate Tilers. Usually
this is what you want but it isn't always.
