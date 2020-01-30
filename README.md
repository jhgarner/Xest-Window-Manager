# Xest Window Manager

Are you a user? Check out Xest's website at https://jhgarner.github.io/Xest-Window-Manager/

Are you a developer? Keep reading!

# The Code

## Who Should Read the Source Code?

Xest is a window manager written in Haskell. The goal of this codebase is to be
at least partially accessible to any other Haskell users. It definitely doesn't
abide by the "boring Haskell" mentality though. In fact, I used this project as
a testing ground for tons of "fancy Haskell" features. It's got Template
Haskell, pattern synonyms, Polysemy, and a massive host of language extensions.
With that said, most of the "fancier" code is confined to a handful of files.
Since I'm the one who wrote it, code that seems straightforward to me might
actually be terrifyingly obtuse. If that's the case, let me know so I can fix
it.

I'm always happy to answer questions about Xest no matter how silly they may
seem to you. Feel free to open up issues or check out my profile for how to
contact me if you have any questions.

## What Should I Know?

The most obvious one is the Haskell language. I really enjoyed the wikibook on
the language as well as the first few chapters of Learn You a Haskell.
Everyone's experience is different though. I've also read plenty of other
websites and blogs as I learned.

Understanding Xlib, ICCCM, and EWMH will also be helpful although they're pretty
comprehensive and I'm still dealing with parts I don't understand. The Haskell
bindings for Xlib map up pretty closely with the C API so any of the docs on
something like Tronche will be extremely valuable. For things like ICCM and
EWMH, I've been learning them as they've been creating problems. The cool part
is that most of the specs don't really apply to minimal tiling window managers.

I would highly recommend reading
https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html if
you have the chance. If there was one thing that changed how I thought about
writing code the most during this process, it's that blog series. Without it, I
probably would have given up on Xest and the mess of code it had created.

Finally, I would recommend reading about Polysemy. I jumped onto Polysemy
because it was new and sounded cool. I've grown to really like it though as a
way of organizing effectful code. If I had to pick a second biggest change to
how I think about code, this library would be it.
