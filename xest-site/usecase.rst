---
title: Home
date: 2015-1-1
---

Using Xest
==========



Startup
================================

After installing Xest, you should be thrown into a mostly blank environment. The
first thing you'll want to do is press the super key.


The pink border that appeared signifies that you've entered normal mode.
Previously, you were in insert mode. You can reenter insert mode by pressing
escape.

Press and release the super key to enter normal mode, then press and release the
enter key. This should open up a terminal on your system.

Notice the text on the border. The @ symbol represents the monitor's location.
Anything to the right of the monitor is considered inside of it and rendered.
The * represents the input controller. The borders you see are drawn around
whatever 

Imagine there are two projects you're working on. One of them is a
research paper and the other is some kind of coding project.

For the research paper, maybe you have a workspace designated to finding
sources in your web browser and a workspace with LibreOffice open.

.. raw:: html
  <video playsinline autoplay muted loop>
    <source src=/images/1.mkv/>
  </video>

For the programming project, you might need a workspace with your browser open
with documentation, a workspace with your text editor, and a workspace with a
couple of terminals for compiling and testing.

Soon, you're wondering how to organize the two projects. Do you put the
research workspaces before or after the project workspaces? What do you do if
you need another workspace for one of them? Is moving between everything easy?
Is it easier just to close everything related to one project while you work on
the other?

The solution to this problem is nested workspaces. At the top level, you have
two workspaces open: one for the research paper and one for programming.
Inside of those, you have workspaces for each project. You can now pretend like
only one of the projects exists at a time making it easier to work without
distractions. You can also make better use of muscle memory. Your web browser
can always be on the first workspace no matter what you're working on.

Although desktop environments like Plasma offer nested workspaces built in, Xest
offers something more generic with its tree structure. In theory, you can nest
workspaces as deep as you want in Xest and you can easily move entire desktops
around.

How stable is Xest?
===================

Although I am currently using it pretty much full time, there are still bugs and
changes to be made.

I try to fix as many bugs as I can, but I can't hit every possible use
case. If Xest crashes, every program you had running will crash as well which
could mean losing work. Make sure to save often! If Xest crashes, check out the
various files with "xest" in the name in your "/tmp/" directory.

I want to try it out!
=====================

If you're using arch, check out the package in the release section of the Github
project. You can install it using "pacman -U <path to tar.gz file>" Once you do
that, follow the instructions in the user guide.

If you want to run Xest from source, clone 
https://github.com/jhgarner/Xest-Window-Manager and install nix. The stack build
tool should make the binary. For actually creating the menus in your display
manager, more work will be needed. Look at the Arch package and try to get the
non-binary files into the right places.

Check out the other pages on this site for information on using Xest.

