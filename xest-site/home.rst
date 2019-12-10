---
title: Home
date: 2015-1-1
---

What is Xest?
=============

Xest is an X11 Window Manager with both tiling and floating support. It is
inspired by other projects like i3 and XMonad.

The defining characteristic of Xest is its tree-like nesting. Xest isn't unique
because it represents layouts and windows like a tree; i3 does this already.
What makes Xest unique is the direct control users have over the tree. This
increase in control allows you to do things like nesting workspaces.

What does this mean in practice?
================================

Imagine you have two projects you're working on. One of them is a
research paper and the other is some kind of coding project. Normally, these two
projects will be hard to organize on your desktop. 

For the research papaer, maybe you have a workspace designated to finding
sources in your web browser and a workspace with LibreOffice open.

For the programming project, you might need a workspace with your browser open with documentation,
a workspace with you text editor, and a workspace with a couple of terminals for
compiling or testing.

The question quickly becomes how to organize the two projects. Do you put the
research workspaces before or after the project workspaces? What do you do if
you need another workspace for one of them? Is moving between everything easy?
Is it easier just to close everything related to one project while you work on
another?

Xest's solution to this problem is nested workspaces. At the top level, you have
two workspaces open: one for the research paper and one for the programming. Inside
of those, you have workspaces for each project. You can now pretend like only
one of them exists at a time making it easier to work without distractions. You
can also make better use of muscle memory. Your web browser can always be on the
first workspace no matter what you're working on.

How stable is Xest?
===================

Although I am currently using it pretty much full time, there are still bugs and
changes to be made. I am still not sold on the primitive operations currently
available for moving around the desktop so those are likely to change. If you
have ideas, I would be excited to hear them.

I try to fix as many bugs as I can, but I can't hit every possible use
case. If Xest crashes, every program you had running will crash as well which
could mean losing work. Make sure to save often!

I want to try it out!
=====================

You can clone Xest from https://github.com/jhgarner/Xest-Window-Manager and run
the install.sh file to get going. You will need to install Haskell's Stack build
tool before installing since I have now idea how universal the precompiled files
are.

Check out the other pages on this site for information on using Xest.
