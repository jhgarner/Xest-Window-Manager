# SRC

Welcome to the src directory! If you've clicked into here, you must be
interested in looking at the code. I've made attempts to keep it clean and
readable but be warned that few others have sanity checked it. In addition,
I've actively searched for some "fancy Haskell" if I thought it would
improve things, so there's a nontrivial learning curve and
probably some unnecessary complexity. The
purpose of this readme is to provide some guidance on what's going on in the
various files.

I'm very willing to accept ideas for how to improve readability and
maintainability so open up an issue if you have any!

## Standard*

This module provides the prelude replacement. It includes some generic types
that could be useful in theory for other projects.

## Config.hs

A fairly simple module which defines some of the data types and functions used
in the config file provided by the user. Dhall is used to read the config file.

## Lib.hs

This is the starting point for Xest. There's a lot of initialization code as
well as the main loop. If you're trying to understand the app's control flow,
start here.

## Core.hs

A bunch of Xlib functions that don't really belong as events or actions. If you
want to know how we actually draw the tree, look in here for the refresh
function. Other than drawing, Core.hs does a lot of EWMH stuff. This should
probably be split up.

## XEvents.hs

Functions to be run when certain X11 events come in. All of these are called by
Lib.hs in the main loop.

## Tiler/

The files in here contain a lot of non-effectful code. The craziest thing is the
TilerF data type. Almost all of the constructors used are actually pattern
synonyms wrapping the versions with F appended to them. These cool smart
constructors will automatically apply the Fix/unfix functions as needed. The
result is a lot less verbosity with slightly more confusing types. If you're
trying to debug something and getting problems with Fix/unfix, try adding
explicit signatures everywhere GHC will let you. I've found that those
signatures force GHC to localize its error much more. From there it's easier to
see where things went wrong.

In addition, using some kind of editor with ghcide/haskell-language-server
helps a ton. Being able to hover over terms to see their types has been a huge
productivity boost.

## Actions/

The two files in here provide the actions that users can bind to keys. A lot of
them involve interesting tree traversals and a mix of pure and effectful code.

## Base/

The files in here define all of the low level X11 code. I've tried to break
things up into reasonable pieces but there are probably still improvements to
make. Other than Lib.hs, the files in here are the only ones that can do actual
IO.

## FocusList.hs

I would happily swap out FocusList with something else. Although it works, it
has neither the safety guarantees it should nor the big O performance it should.
Many of the primitive operations on FocusList are complicated to implement and
require traversing the entire data structure.

One idea for improving the external interface it provides is to use lenses.
Another is to replace the indexed list implementation with something like a
bidirectional graph. If I go that route, lenses and pattern synonyms will be
important for making the code that uses FocusList idiomatic.

It's kind of poorly commented. The time that would be spent improving its docs
should be spent rewriting it instead.

## TH.hs

Ever wanted to know what it looks like to write macros in Haskell? This file
creates all of the Tree constructors that don't have F appended to them. This
file is pretty much orthogonal to everything else so don't worry too much about
it. It's got tons of comments though if you want to look inside. In general, I
found writing the TH code relatively painless once I gave up on quasiquotes and
Coercible.
