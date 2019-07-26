# Recursion

## Where is it?
If you peruse the Xest source code, you will notice a lack of recursive
functions. The few places with explicit recursion are either marked TODO or need
to be. Instead of using recursion explicitly, Xest opts for recursion schemes.
I would highly recommend reading through
https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/ for a great
overview of what they are. Although the series covers plenty of fascinating
stuff, cata and ana are the main ones used by Xest with para coming up twice.

## Why?

The above blog post makes the claim that recursion is analagous to goto in
imperitive code. Likewise, recursion schemes are analagous to if, else, while,
for, etc. I've decided to buy that claim so using recursion explicitly is seen
as bad style in Xest. This should allow anyone working on Xest to gain the
benefits recursive thinking offers while providing the simplicity non-recursive
functions give.

## A brief overview

Given a recursive data structure, cata allows you to bubble up value from the
leaves up to the root. Cata takes a function which turns a Tiler of something
into that something. For example, Using cata to find the number of Tilers in
the tree might involve a function which takes a Tiler of Ints and returns 1+ the
sum of Ints after adding one to it. Such a function could be written as 
"(1 +) . sum" and would be used as "cata ((1 +) . sum) tiler".
