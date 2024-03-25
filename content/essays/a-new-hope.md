+++
title = "A New Hope"
date = 2024-03-25
+++

What am I trying to solve?
 + Developer productivity while tackling new/complex problems
 + Ability for a single developer or small teams of developers to tackle novel/complex problems
 
What stands in the way of this?
 + Long feedback cycles
 + Ancillary work that has nothing to do with the problem at hand
   (managing files, organizing code, managing versions, etc)
 + Bad tooling for exploratory programming
 + Bad introspection of the state of the system
 
---

I am very bad at planning or thinking about how a program should look
before I start writing. I'm sure a lot of other people are in that
boat as well. I'd even venture to say that all developers have no idea
what the program should look even if they think they know the
problem. Well, that's probably not even the root of the issue. Once
you have a problem statement, I'm sure no-one even knows what the
problem is at that point. Only after trying to solve it for a good
while you start mapping the problem in your head and things start
making sense. I'm sure some quote exists about throwing the first
program you write for a particular problem, but I cannot recall it.

In my case the only real way I can tackle a problem is to write code,
the same way some people structure their thoughts by writing it down
as an essay I do the same with code. I make an educated guess of what
a possible representation of the problem may be in my language of
choice, and I iterate over that. I create some inputs and some
outputs, I create some functions that incrementally move from one to
the other. Internally I have a tentative path, but it's very
foggy. Only after bashing my head over and over I find a tortuous path
between one and the other. Then with a clearer map of the territory i
start to find a less tortuous path over it. Depending on the problem I
may be satisifed at this second iteration, usually I take a lot longer
to reach something that I think is good enough.

If I don't do this, I'm unable to provide a good codebase. I will
provide something that kinda works but that is filled with
inconsistencies, hard to maintain and a mess.

*I think I'm not alone in this.*
 
The problem is that most development environments are not designed to
help with the exploratory nature of this way of developing
programs. It seems to be designed around what the end-result needs to
look like. Well-structured files in clean hierarchies and packaged
within cleanly delimited interfaces and modules. Who can write those
from the start? My guess is no-one. Of course as we get better and
better at programming we've seen more and more problems that look
similar and we can probably find a solution that is pretty close to
that ideal end-result a lot faster. But that's not the case when we
are treading new territory.

When attempting to interactively discover and map a problem space, the
development environment works against me. It asks me to start by
creating a file, and writing some code. Most environments also ask me
to create a main entry point and then setup some type of build
definition. Then to start exploring the problem I create some
structures that I think represent the problem space, then some
functions that operate on those structures. Then I need a way to be
able to see how the functions operated on those structures to see if
they do as I expect them to do.

Of course some environments already provide or improve on some of
these parts. Python can print any of their basic structures by
default, an improvement over C/C++ where you need to provide those
yourself. Rust goes a step further and can automatically derive a
Debug print by inheriting a trait on any datastructure you
create. With python I can somewhat create and modify the state of the
system from the repl but if I use classes I cannot really update them
with new fields without losing the existing state.

Some framworks have improved on this, providing live reloading when
you change the code, but most of the times they cannot keep the
state. They expect the state to live in a DB and the code to be
stateless. This works fine in some projects, but not all projects need
a DB. And even if they do, this adds a new source of rigidness to our
system. Evolving the database schema, which is, in most systems, very
cumbersome.

**If we had better exploratory programming tooling we could tackle
more complex problems with less people**

There you go, that's my thesis. We need to have better, interactive,
exploratory and *playful* development environments.

After years of developing in other languages, lately I've been leaning
into Common Lisp a lot more. It provides a lot what I expect by
default and the language was clearly designed with interactivity in
mind. Together with emacs+sly or the propietary LispWorks you can
build systems interactively without ever stoping to reload, recompile,
or recreate the state. What a breath of fresh air. It has quickly
become my favourite development environment and I cannot understand
how languages developed after do not have some of this things. There
is no reason python could not do a lot of these things.

There are of course shortcomings to doing this in Common Lisp. The
ecosystem is not the same as in Python or Rust. Lots of libraries are
missing, those that exist have very bad documentation if they even
have documentation at all.

The amazing thing about Common Lisp as a development environment is
that it covers the whole range, from exploring to production. It
covers the high level, live reloading, macro writing, interactive side
and the performance-oriented, debugging, fast, robust side that you
want in order to deploy your systems.

We also need to be able to bridge the gap between the exploratory
side, and the productionization side easily. One of the problems I've
found is that I've built an interactive example, and then you need to
do a lot of work to move it into the production direction. And I'm not
talking about essential work, but about now setting an environment
that leads to a productionized product. Build systems, organizing the
code, setting up a CI. That once again has nothing to do with the
problem at hand. It's ancillary work that you need to transition from
one to the other. And while it's fun to build these things, when you
want to deploy something and to turn your idea into something that
other people can use, it just becomes friction.

## Problems with the current system

For me, Common Lisp represents the current local maxima. The system
that allows me to do my exploratory programming and productionize it
mostly in peace. But it has it's shortcomings.

### File based

It is still file based, I still need to decide what goes where. We
want things that are modified together to be together. Lots of time is
spent organizing code so it's easier to find later.

**I should not really care about the file abstraction.**

It's an implementation detail. I know some people have tried this
before and I know there are non-file based systems. I'm sure this one
will fail too, but, tbh I'm only doing this to get myself MY ideal dev
env. If I manage to be more productive myself, maybe I shouldn't
consider it a failure.

A bit of a side note, somewhat unrelated to the current topic. Some
stream of consciousness that I've been thinking lately: Everytime I
start a project that I think that should already exist I wonder the
same thing, *why doesn't it exist?*. Am I doing something that is
idiotic? Something that no-one wants? Something that is unfeasible?.

Back to the topic at hand and related to the file abstraction, it's
very hard to keep the interactive environment and the files in
sync. When you test things in a REPL, if you forget to bring them back
to the file then you've lost that. You need to constantly keep track
of what version of the interactive thing you've built you currently
have active, as well as which ones you want to keep. This is another
friction that kinda seems self-inflicted, if you are doing things
interactively, you may lose them.

### Interactive problem space

Another issue relates to how evolving a system means losing the
previous program state somewhat. It's hard to develop a program as a
tree of design decisions and then navigating the tree. You want to
explore a particular design decision, but what if it was a wrong turn
and you want to go back? It becomes very hard to recursively explore
the problem space without having to create a lot of duplicated work or
doing some manual work to keep track.

You can of course use current existing tools for this, any version
control system allows you to test these things safely, but it's not a
very good solution for interactive development. You lose program
state, you need to have the changes in a file (again), you need to
make sure that you have everything registered and you need to give it
a name.

I don't know if I'm the only one but when I'm exploring a problem
space, it's very hard for me to decide when I want to take a snapshot
of the current state. I don't really know if I will want to return to
a particular point, I tend to forget very easily that I may wanna
return to a particular point and I never commit anything.

**What if we could add interactivity to version control as well.**

## An idea for an alternative

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

There are two key elements of this new system:

 + Non-linear editing of the source: Not file based, you have a
   database of symbols and you can bring them and hide them as you
   please. There is a concept of a workspace, where you can see only
   the definitions for the symbols you need and nothing else. These
   workspaces can be saved and restored. That way you can have
   different "Notebooks", depending on the problem you are solving.
 + Per-symbol history, you have a tree of versions for a particular
   symbol. You can navigate the tree, go back in time. This tree is
   kept automatically, everytime you re-define a symbol we save a
   snapshot. You can diff between different states and even merge
   versions.
   
The package abstraction of Common Lisp being basically a bag of
symbols, some of them external works wonders for this, it seems to be
tailor made for it. Also being able to load and compile code at
runtime allows us to do all of this with very little code and very
little complexity.
   
We no longer need to rely on files, everything is stored in an SQLite
database. An uniform storage medium that can be queried and loaded on
demand. It keeps the package notion of standard Common Lisp and for
most Common Lisp programmers it should feel very natural.

### Collaborative Explorative Programming

Saving the code in a database allows us to build even crazier ideas.

Instead of a SQLite it could be a shared postgres, then you could have
a collaborative explorative programming environment. Imagine
inspecting the changing graph of problem space exploration with a
couple of friends. It would be trivial to save your current state,
switch a function to whatever your friend is working on, test it out,
merge it, return back to what you were doing and continue adapting it.

You could literally follow as a person develops by tracking the loaded
symbols of another user of the system. And test things while they
work.

If the system also tracked dependencies between functions you could
bring a function and all their dependencies to a particular version
while leaving the rest the same.

There are a lot of operations that you could do on that and a lot that
could be explored in that space.

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

## Unanswered Questions and Future Work



One interesting idea I have is to be able to set tests for the
functions and be able to run them across versions.

