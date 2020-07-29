---
title: Home
---

# What is Xest?

Xest is an X11 Window Manager with both tiling and floating support. It is
inspired by other projects like i3 and XMonad.

The defining characteristic of Xest is its explicit, tree-like nesting. Xest
isn't unique because it represents layouts and windows like a tree; i3 does that
already. What makes Xest unique is the direct control users have over the tree.
This increase in control allows you to work with things like nested workspaces.


# What does this mean in practice?

Imagine there are two projects you're working on. One of them is a
research paper and the other is some kind of coding project.

For the research paper, maybe you have a workspace designated to finding
sources in your web browser and a workspace with LibreOffice open....

<video playsinline autoplay muted loop>
  <source src=/images/research.mp4>
</video>

For the programming project, you might need a workspace with your browser open
with documentation, a workspace with your text editor, and a workspace with a
couple of terminals for compiling and testing.

<video playsinline autoplay muted loop>
  <source src=/images/codeproject.mp4>
</video>

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

<video playsinline autoplay muted loop>
  <source src=/images/nested.mp4>
</video>

There's a little bit more going on in this video compared to the others. The
important part is that you can reuse workspaces with only a couple
extra key presses when switching projects. Once you've switched projects,
everything is muscle memory.

Although desktop environments like Plasma offer nested workspaces built in, Xest
offers something more generic with its tree structure. In theory, you can nest
workspaces as deep as you want in Xest and you can easily move entire desktops
around.

# How stable is Xest?

I consider Xest stable enough to use daily on both my laptop and desktop.

I try to fix as many bugs as I can, but I can't hit every possible use
case. If Xest crashes, every program you had running will crash as well which
could mean losing work. Make sure to save often! Check out the
various files with "xest" in the name in your "/tmp/" directory if something
goes wrong. All logs get output there.

# I want to try it out!

If you're using arch, you can grab the package from the
[AUR](https://aur.archlinux.org/packages/xest-window-manager-git/). You can
install it using yay with "yay -S xest-window-manager-git". Since it builds the
package from source, the first install might take a while. Updates will go much
faster.

If you want to run Xest from source, clone
https://github.com/jhgarner/Xest-Window-Manager and either install nix or setup
the dependencies yourself. The stack build tool should make the binary. For
actually creating the menus in your display manager, more work will be needed.
Look at the Arch package and try to get the non-binary files into the right
places. I can probably throw together a package for your distro if you send a
message on the Matrix chat.

If you're interested in Xest, check out the user guide and join the Matrix chat.