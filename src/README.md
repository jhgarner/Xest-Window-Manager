# SRC

Welcome to the src directory! If you've clicked into here, you must be
interested in looking at the code. I've made some attempts to keep it clean and
readable but be warned that few others have sanity checked it. In addition, I've
actively searched for some "fancy Haskell" if I thought it would improve things,
so there's a nontrivial learning curve and probably some unnecessary complexity.
The purpose of this readme is to provide some guidance on what's going on in the
various files. If anything doesn't make sense when looking inside the files,
feel free to open an issue.

I'm very willing to accept ideas for how to improve readability and
maintainability so open up an issue if you have any!

## Standard*

This module provides the prelude replacement. It includes some generic types
that could be useful in theory for other projects.

Some of the changes are a little nonstandard. For one, `fmap` is just called
`map` and a handful of partial functions are now total using either new
typeclasses or wrapping the output.

One thing I'm not happy about in this code base is the use of
`fromMaybe (error "Shouldn't happen)`. It seems like there's a way to convince
the type checker that those cases won't happen but I don't know how.

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
function. Other than drawing, Core.hs does a lot of EWMH stuff. This file should
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
explicit signatures or type applications everywhere GHC will let you. I've found
that those signatures force GHC to localize its error much more. From there it's
easier to see where things went wrong.

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

Base defines a bunch of typeclasses used by the Capability library. Capability
is not a particularly well known package so I should probably provide some context
about whey I use it.

Near the beginning of the project, Xest had something that looked a lot like the
X monad. In an attempt to refactor the project and make things less unwieldy,
Xest switched over to Polysemy. Polysemy was great but it had a memory leak that
crippled Xest after running for a while. Xest then switched to an MTL
style but with a massive stack and overlapping instances. Instead of having a
single large state object, Xest had a large number of stacked `StateT`
transformers where each stored a different part of the state. Unfortunately,
this design brought out a GHC bug that made compiling with optimizations
infeasible. A fix for that bug should be in GHC 9.0. Until then, Xest uses
Capability to flatten the transformer stack into a single large record.

If someone wanted to replace Capability, I wouldn't be opposed. Everything is
pretty well abstracted by the `Member` type function so it's likely not too
extensive of a refactor.



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

On a couple of occasions I've tried to completely replace the FocusList
implementation, but I've always given up on trying to debug the new
implementations. The problem appeared to have been that the interface to the
focus lists leaks a lot of details about the implementation. I've started
replacing the interfacing with more generic combinators although that work is
definitely not complete.

## TH.hs

Ever wanted to know what it looks like to write macros in Haskell? This file
creates all of the Tree constructors that don't have F appended to them. This
file is pretty much orthogonal to everything else so don't worry too much about
it. It's got tons of comments though if you want to look inside. In general, I
found writing the TH code relatively painless once I gave up on quasiquotes and
Coercible.

It also has some code to reduce the duplication required by the Capability
library and the lack of partially applied types in GHC. I'm not totally sure
that the end result is more readable and am happy to consider other options.
