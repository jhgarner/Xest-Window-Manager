![Logo](https://raw.githubusercontent.com/jhgarner/Xest-Window-Manager/master/xest-site/images/xestlogo.png)


# Xest Window Manager

Are you a user? Check out Xest's website at https://jhgarner.github.io/Xest-Window-Manager/

Are you a developer? Keep reading!

# The Code

## Who Should Read the Source Code?

Xest is a window manager written in Haskell. The goal of this codebase is to be
at least partially accessible to other Haskell users.
Since I've been the only contributor though, there is almost no way that
goal has been met. It's much more likely that the code is filled with strange
patterns and obtuse logic. If you're trying to understand the code and something
doesn't make sense, let me know or open up an issue.

I'm always happy to answer questions about Xest no matter how silly they may
seem to you. Feel free to open up issues or check out my profile for how to
contact me if you have any questions.

## What should I know if I want to contribute?

If you've never seen the Haskell language before, jumping into this codebase
is probably a bad idea. If you have a little bit of experience, I would
recommend taking a look at some of the files in the src/Tiler/ folder as well as
the src/NonEmpty.hs and src/FocusList.hs files. All of
the functions in there are "pure" in that they only modify internal data
structures.

The weirdest things you will find in those files are the uses of "Fix" and "cata". 
I would highly recommend reading
https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html if
you haven't seen them before. That blog series fundamentally changed how I
thought about recursion and ADT's. I likely would have given up on this project
if I hadn't found that way of organizing the code.

If you want to look at the other parts of the codebase, you'll need a basic
understanding of what the X11 protocol is. I would recommend
https://magcius.github.io/xplain/article/ and
https://jichu4n.com/posts/how-x-window-managers-work-and-how-to-write-one-part-ii/
as good descriptions of X11 and how window managers work. The Haskell X11
library is pretty close to the original xlib so any docs related to xlib will
probably help as well. I found myself using the Tronche website a lot for xlib
documentation as well as the XMonad source code.

ICCCM and EWMH are protocols built on top of X11 and tend to get sprinkled
around the src/XEvents.hs code. The official documentations for those protocols
are probably the best resources for learning about them.

If you're looking at the code that interfaces with the X server, you will
probably run into Polysemy. Polysemy has pretty good official docs although you
can safely ignore a lot of the more complicated concepts like higher order
effects as Xest doesn't use them.
