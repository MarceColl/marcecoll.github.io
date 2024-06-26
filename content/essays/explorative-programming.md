+++
title = "Explorative Programming"
date = 2024-03-26
+++

I am very bad at planning and visualizing what a program should look
before I start writing it. I'm sure a lot of other developers are in
that boat as well. I'd even venture to say that (almost?) all
developers have a hard time doing that even if they think they
understand the problem. Well, that's probably actually the root of the
issue. Without writing or tackling them, it's hard to understand
problems. You can only reach so far by thinking very hard in
isolation.

In my case the only real way I can tackle a problem is to start
writing code and playing with the problem and its data. I can talk
about how I approach the process at another time, but I need to explore
the problem space to find its limits, its edge cases, what areas are well
lit and which are in shadows. I cannot think of this in the abstract.

*I think I'm not alone in this. Hopefully?*
 
The problem is that most development environments are not designed to
help with the exploratory nature of this way of mapping a problem
space. They seem to be designed around what the end-program needs to
look like. Well-structured files in clean hierarchies and packaged
within cleanly delimited interfaces and modules. Who can write those
from the start? My guess is no-one.

There is a *rigidness* in our current development environments that
makes this important first step a lot harder than it should be. This
rigidness actively works against us, and prevents us from
understanding problems.

## Rigidness

What are the sources of this rigidness? I believe there are several and this
is not a exhaustive list, just the ones that I find most annoying and the ones
I've been looking for solutions for.

 + **Long feedback cycles**, caused mainly by two things:
   + **Build steps**, if you need to recompile or rebuild the system,
     it incurs a cost everytime you try something new.
   + **Loss of program state**, even if your language doesn't need a
     build step, most languages are not designed to keep your program
     state evolving as you evolve the code and their data structures.
     This is a huge source of friction, and it's a huge cost to pay
     every time you wanna try something new.

 + **File based organization**, right from the start you need to start
   deciding what goes where. Of course I think most of us start with a
   single file, full of random functions and tests. But at some point
   this file becomes unbearable, impossible to find what you are
   looking for. At that point you have to make decisions and start
   splitting the file at logical points. Deal with what is private and
   what is not. It may not seem like much, but it's very annoying.

 + **Bad system introspection**, most languages require you to
   implement text-based outputs for your internal data so you can
   actually inspect what is going on.

 + **Bad solutions for tree-based exploration**, there are usually
   several directions that look good when tackling a problem and once
   you decide on one of them it's very costly to try the rest. The
   version control systems we have are not designed for this kind of
   work, they require the user to create snapshots when the user
   wants. If they have forgotten to do it at some point, that snapshot
   is lost. And I can just speak for my experience here, but while I'm
   doing this exploratory work I never know what I wanna save and what
   not and what avenue will be worth exploring. I don't wanna be
   thinking about any of that either.

 + **Bad Collaboration Story**, the same systems I talk about in the
   last point are also the way we do collaboration in software
   development right now, tools like `git` or `mercurial`. If they are
   a failure for tree-based exploration, then they cannot work to
   collaborate on exploration. They are already lacking the needed
   info for collaborating.
   
This rigidness is creatin so much friction when it comes to tackling
new and complex problems, both alone and in small teams. It requires
so much upfront investment that I think we are missing out on a lot of
new and interesting ideas because it's costly to do so.

**If we had better exploratory programming tooling we could tackle
more complex problems with less people**

There you go, that's my thesis. We need to have better, interactive,
exploratory and *playful* development environments.

## A step in this direction

Recently I've been spending more and more time writing Common Lisp
after years of writing Python, Rust, C and Javascript/Typescript.  And
it's not only because of things like macros and definitely not because
of the ecosystem. It's because it's the environment that more closely
matches my development process. The feedback cycle is immediate, it
allows very good introspection of every object in the system, and it
makes a lot of effort to keep your program state after changing
anything[^1].

The amazing thing about Common Lisp as a development environment is
that it covers the whole range, from exploring to production. It
covers the high level, live reloading, macro writing, interactive side
and the performance-oriented, debugging, fast, robust side that you
want in order to deploy your systems.

It still requires you to use files, working in the REPL-only brings
its own set of problems. REPL and files are hard to keep in
sync. That's another thing that what I'm development helps with, if
you remove the files, there's nothing to keep in sync.

And it offers no help for the last two points[^2] I made above.

## An idea for what comes next 

I've started work on an alternative system. It's not currently in a
usable state but it's in a working state and I'm fairly close to
dogfooding it.

I've decided to use Common Lisp as a base language for this
development environment. I didn't want to have to create a whole
language from scratch to test out these ideas, I tend to get into
useless rabbitholes that detract from what I'm trying to do, not this
time. Common Lisp as I said already has what I consider to be the
local maxima for my workflow and has the tools to extend the language
in the direction I want. It's the perfect testing grounds.

There are two features I'm starting with in this system:

 + **Non-linear editing of the source** Getting away from file based
   organization, you have a database of symbols and you can bring them
   and hide them as you please. There is a concept of a workspace,
   where you can see only the definitions for the symbols you need and
   nothing else. These workspaces can be saved and restored. That way
   you can have different "Notebooks", depending on the problem you
   are solving, with only the definitions you need at any given time.
   This solves the code organization problem and reduces the friction
   of starting something.

 + **Per-symbol history**, we keep the history of every single version
   of functions, `defvar`, `defparameter`, `defclass`, `defmacro`, etc
   you have created and sent to the REPL. You can navigate the tree,
   inspect the history, go back in time. This tree is built
   automatically as you re-define any symbol. You can diff between
   different states and even merge versions. This works at per-symbol
   granularity. This attempts to solve point number 4. You can recurse
   into design sub-trees and go back in time without needing to manage
   history manually.
   
Currently all of this is implemented by having all symbols in an
SQLite database and with a custom Emacs mode. You have a tree of
definitions and we have mappings of `(package, symbol) -> definition`.

Common Lisp makes this extremely easy, packages are nothing more than
a bag of symbols, some public, some private and there are objects
assigned to symbols. So we are working on the same package abstraction
that common lisp is already based on. Backed by a DB instead of
in-memory in the Lisp listener. We don't even lose performance since
Common Lisp exposes the `compile` function to compile lambda forms.
   
### Collaborative Explorative Programming

Expanding this idea a bit more, and to tackle point #5 we could
replace the SQLite with a shared database. Instead of having one set
of mappings you could have one per user while having a shared history.

In order to do this having ways to delete, merge and search the
history database would become needed since the amount of code could
get unwieldy pretty fast. One interesting thing we could do to search
for valid history entries is to be able to create tests for a symbol,
locally but shareable and to be able to run those tests on all/some
history entries. You could then see which ones match your
specification or for which tests it fails. It could help narrow down
what parts of the tree were valid and which not. With this you could
even dynamically query the history tree by creating ad-hoc predicates
over inputs and outputs of the functions.

Other interesting ways of developing in groups withh this could be to
allow you to live-track the dev env of another user. Following the
same set of symbol mappings, you could even limit it to certain
functions or function trees (if we kept track of which functions
depend on which functions). That way your friend could be working on
function A while your work on function B and you both have constantly
updated versions of each others functions while you work on yours.

This is not implemented for now, but it would be a natural
extension to the system.

### Live deploy

Another interesting thing that we could develop is the ability to
deploy code on a live instance in a way that automatically rolls back
if too many errors happen.

This maybe is too crazy, since there is a reason why we've converged
into deploying dumb stateless services in docker images. We don't wanna
deal with a changing environment. But, is it really that different?

Imagine a service that never stops, you diff the existing functions
with the new functions, only updating what needs to be updated. If
there is a change in the rate of errors then you automatically
rollback the deployed functions.  A smarter system could even track
which functions changed their error rate and only rollback any
function that is related in that execution tree that changed in the
last deploy.

It gets harder to track, but maybe it's possible.


## Concerns and worries

I have two main worries and areas of the system that I'm unsure how I'll tackle:

 + How to interact/merge this methodology in existing systems that are
   file based in a way that doesn't add a lot of friction back.

 + At the top-level of any common lisp program there may be
   expressions not associated with a symbol. I'm very unsure how to
   tackle that if at all. I tend to always wrap everything in
   functions anyway, but I don't want to impose a particular way of
   programming if you wanna use the tool. I need to give this more thought.
   
And the final worry, which I think a lot of us have when tackling a
new design space:

 + Why hasn't been done yet before? Is it a bad idea? Does it have
   hidden complexities that make it impractical? Do people not develop
   like this? Will the idea not be as useful as I expect?
   
 I guess there is only one way to know :)

## Conclusion

This is a topic very dear to my heart, it has taken me many years to
come to understand how I best develop software. I'm not a very
introspective person in general and it has been a long journey.

Hopefuly some of it was interesting and I look forward to learn more
about if these ideas resonate with you and what the problems may be.

I will write another post at some point to show how all this works
from within. It is actually surprisingly small because of the amount
of tools that CL gives the user, but there are some interesting
challenges nontheless.


[^1]: CLOS evolves the instances of any class you define as you evolve the class definition

[^2]: I guess you could have two people remotely controlling the same
    repl through swank, but that would get unwieldy quickly
