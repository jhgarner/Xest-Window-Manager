# X11 and Window Managers

## Recommended reading
https://jichu4n.com/posts/how-x-window-managers-work-and-how-to-write-one-part-ii/
https://magcius.github.io/xplain/article/

First, I would recommend reading through both of those as they explain
X11 much better than I would. Those were also some of the first resources I
found useful when starting the project.

What follows will primarily be a summary of those resources and how they relate
to Xest. The first covers
how you can start writing a window manager in C++. The second is a more general
series on how X11 works under the hood with plenty of interactive examples.

## Servers and Clients

X11 is a protocol with several implementations. The most popular is Xorg, but
others exist. Xming and Cygwin provide X11 servers on Windows. Another one, which is
extremely useful on Linux, is Xephyr. Xephyr creates a nested X11 server as a
normal window inside another server. Think of it like X-ception. 
When testing Xest, I usually start up a Xephyr session
and have Xest operate inside of that. Note that Xest is not a server; Xest,
as well as Gnome, i3, and all the other window managers, are just clients to the X11 protocol. When
creating a window manager, you use the same Xlib anyone else uses.

If you start up Xephyr, a terminal, and change the Display environment variable
in that terminal, you can run applications and see how they act without a window
manager. You may be surprised by how much works. Using Feh you can get a
wallpaper. Then you can launch programs like Firefox or Rofi and have focus follow your
mouse. This should make it clear that the purpose of a Window manager isn't to
manage operations inside windows. That's handled by the server. The window manager
will be in charge of moving those windows around and drawing
decorations.

As a tangent, this means the term "server side decorations" doesn't really
describe what is actually happening. It isn't the X server drawing those, but another X
client. A small technicality but an interesting piece of trivia.

## Windows and Displays

When you start up an X server, you create a new Display. Calling it a display can be
confusing though since an X display doesn't correspond to a physical display. Displays
are given numbers when they're started. The first is Display :1 and is likely the
only one you have running. Each server has one display associated with it. I can
have i3 running on Display :1, then start Xephyr and Xest on Display :99. Gnome can
be running in Display :1 on multiple monitors and Xephyr can be running as a window spread
across all of them. X also has a concept of screens which are like actual
monitors, but it still gets a little weird.

Windows are inside of Displays. All but the root window (we'll cover that later)
have one parent. Windows can have other windows inside of them. In an early
vision for X11, windows would represent things like buttons or text boxes as
little child windows. This may sound familiar to web devs. Unfortunately, there
were enough problems with nested windows that developers moved away from them.

Along with their contents, windows have some meta-data as
well. Some of that meta-data is organized in properties. Although the details are
a little different, a window's properties are essentially a dictionary of
strings to bytes. The creators of X11 realized how powerful this could be, so
they left any kind of semantics around how to use properties outside of the
protocol. Instead, those are defined in ICCCM and EWMH as a form of inter
process communication. Think of properties as global variables shared with
everyone.

Event flags on windows are another important piece of metadata. We can set flags
to be alerted when our window gains focus, when the user types something,
when a child window changes, etc.

It's important to note that properties and flags aren't secured in any way. There is
nothing preventing a random process from clearing, adding, or reading properties
or flags belonging to another window. This lack of security around windows shows
up all over the place. If you have a handle on the window, which the server will
gladly give to you, you can do anything you want. On the one hand, this gives us
extreme power; many of the complaints against Wayland are in its beefed up
security. On the other hand, it opens our computers up to easy attacks.
Regardless, this is what we have and is largely the reason window managers are
able to exist the way they do.


## The root window

X11 is based on the idea of a tree. When you start up the server, there is a
single root window in a display with nothing in it. Knowing that there will always be a
single root to this tree allows for plenty of powerful features. For example,
the root window usually shows your wallpaper if you use feh. It is also used to
communicate between the window manager and other clients through properties. For example, by
writing the number of workspaces to the root window along with their names, you
can offload rendering complex UI elements to things like Polybar. Note that this
isn't part of the X11 protocol. The EWMH standard, created over a decade after
X11, established this convention.

When a new window like Firefox or Xterm wants to create a new top level window,
they make the root window their parent. This creates a tree with the root
window, all top level windows attached to that, and internal windows attached to
those. The top level windows, aka the windows which are direct children of the
root, are the ones window managers should touch.

## Tying it all together

In the last sections, we talked about flags. Two of the
available flags alert you whenever a child window is added, removed, or
modified. Those flags are SubstructureNotifyMask and SubstructureRedirectMask.
The first allows us to receive notifications when a child window does
something. For example, we can be notified when a window is destroyed. The
redirect flag allows us to intercept requests to the X server involving child
windows and replace them with our own. For example, we can intercept a request
to map a window and deny or grant it. 

I also mentioned that flags aren't protected in any way. Since flags aren't
protected, any program, including Xest, can ask for those flags on the root
window. As long as we're the first to request them, the server will grant the
request.

Some window managers do more complex things such as changing the parent from the
root window to something else, or rendering window contents to their own
buffers. The first is called reparenting and is used to add window
decorations. The second is compositing which adds wobbly windows and other fun
(and useful) things. Xest
doesn't do either because they add complexity. In the case of compositing, not
doing it means you can use something like Compton alongside Xest. Examples of programs
which do both include Gnome and Plasma.

## What does Xest do with those flags?

Once the flags have been set, Xest enters a main loop (found in Lib.hs) which
executes actions as they are added to an event queue. When the event Queue is
empty, it blocks and waits for the X server to send a new event. After an
event has been received, it gets processed by the correct handler.

One such handler takes care of the MapWindow event. When a window asks to be mapped,
we add it to a data structure which keeps track of all active windows and where they
are located. Xest then resends the map request as a sign to the server that its okay
with the event happening.

## Conclusions

1. Anyone can access or modify anything related to a window.

2. X is kind of like your browser's DOM but worse.

3. A window manager is just another client in the server's eyes.

4. When X11 was established, modern computers were not the primary target.
   The spec is designed for low memory devices and more innovative compositing
   isn't possible.
