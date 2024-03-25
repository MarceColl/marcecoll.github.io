+++
title = "An experimental Development Environment"
date = 2019-11-27
+++

Development environments of today look not that different from the ones from 10 or 20 years ago.
Sure, we have different tooling, different languages, we use a lot more tooling but the core remains
mostly the same. Organizing code in text files, edit those files using a text editor, using those text 
files as input for an interpreter or compiler, check the files into a source control system. Once you have
something you like you commit it, and at some point generate a build and deploy it.

There are of course differences between environments and what you can do in them, but these are mostly the
same.

## A Survey of Interactive Development

At this point, for me there is no better development system than interactive programmming, perfectly exemplified
by one of the Lisps, my lisp of choice is Common Lisp and everytime I refer to Lisp in this essay I'm refering to it
but probably a lot of this applies to any other Lisp.

The ability to continuously evolve the state and the code of the program while doing exploratory programming is
THE superpower of Lisp in my opinion. I don't know about you, but I cannot really plan very well in advance and I need to explore
the problem space interactively to get a good hang on what the problem and the program is about.

A lot of design of Common Lisp goes towards enabling this interactivity, CLOS the OOP library of Common Lisp allows you to redefine
classes at runtime and it will update existing instances with the new slots (properties). You can add and remove methods at runtime.
You can also code what updating the instances means if you want something more custom than what is the edefault.

You can redefine functions at runtime, the condition system allows interactivity when dealing with errors. When something happens you
can continue right where you left off.

## An idea for an alternative

Recently I was thinkin
