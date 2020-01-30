# SRC

Welcome to the src directory! If you've clicked into here, you must be
interested in looking at the code. I've made attempts to keep it clean and
readable, but be warned that few others have sanity checked it. The purpose of
this readme is to provide some guidance on what's going on it the various files.

## Standard.hs

This module provides our prelude replacement. Most of the interesting work was
done by Classy Prelude which completely re-imagines the container hierarchy.
If you scroll down far enough, you'll find some custom made functions and data
types that seem useful enough to have in every file.

The more I use Classy Prelude, the less I'm sure that MonoTraversable is the
right way to represent containers.

## Config.hs

A fairly simple function which defines some of the data types used in the config
file provided by the user.

## Lib.hs

This is the starting point for Xest. There's a lot of initialization code as
well as the main loop. This file should be fairly simple as most of the
complicated stuff is deferred to the other files.

## Core.hs

A bunch of Xlib functions that don't really belong as events or actions. If you
want to know how we actually draw the tree, look in here for the refresh
function. Other than drawing, Core.hs does a lot of EWMH stuff.

## XEvents.hs

Functions to be run when certain X11 events come in. Originally, there was a
giant "handler" function which pattern matched on all of the event types. Since
then, that's been broken up. All of these are called by Lib.hs.

## Tiler/

The files in here contain a lot of non-effectful code. Since there's no
Polysemy, it's also much closer to "boring Haskell". The craziest thing is the
TilerF data type. Almost all of the constructors used are actually pattern
synonyms wrapping the versions with F appended to them. These cool smart
constructors will automatically apply the Fix/unfix functions as needed. The
result is a lot less verbosity with slightly more confusing types. If you're
trying to debug something and getting problems with Fix/unfix, try adding
explicit signatures everywhere GHC will let you. I've found that those
signatures force GHC to localize its error much more. From there it's easier to
see where things went wrong.

## Base/

The files in here define all of the Polysemy code. I've tried to break things up
into reasonable pieces but there are probably still improvements to make. Other
than Lib.hs, the files in here are the only ones that can do actual IO.

## NonEmpty.hs

This file probably shouldn't exist as it just re-implements non empty lists. Why
have it then?  Classy Prelude provides a NonNull data type but that won't work
for me because it doesn't implement things like Functor. The NonEmpty type from
Base would be cool but a lot of work would be needed to make it import nicely
with all of the Classy Prelude functions and type classes.

If you look inside, there won't be any real comments but it all uses pretty
boring Haskell so hopefully it doesn't look too bad. Any time spent adding
comments should probably be spent moving to Base's NonEmpty instead.

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
