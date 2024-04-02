+++
title = "Delivering a LispWorks application"
date = 2024-04-02
author = "Marce Coll"
+++

WARNING: This is a living document. As I understand more of the
LispWorks delivery system i'll add more.

## Changelog

`2024-04-02` - Added article

## LispWorks

I recently got my [LispWorks](https://www.lispworks.com/) HobbyistDV
license in order to productionize a couple of my Common Lisp application
development. SBCL is amazing but as I am planning to develop a couple
of services that may potentially get into commercial territory I
wanted to get familiar with the commercial CL implementations in case
I need them. The IDE and CAPI are also amazing and I've already
developed a couple of personal GUIs to help me with some of my
workflows.

Yesterday I set out to deliver my first server application, let's
build our understanding bit by bit.

LispWorks has [extensive
documentation](https://www.lispworks.com/documentation/lw80/deliv/deliv.htm)
on the delivery system. You can check there for more details.

## The Delivery System

The LW delivery system allows you to take a common lisp application
and generate a self-contained binary with everything it needs to run.

The system basically takes an image (lisp program state in memory) and
turns it into an executable that contains the necessary LispWorks
runtime to run it. This already gives us a hint of what we will have
to do. We have to setup the image at build time.

## The deliver script

The deliver script is a lisp file that does just that, it sets up the
image as you need it for your final program. Here you load all your
code, all your dependencies, enable any features you need (we'll get
to that later), and finally call the `deliver` function with some
parameters. You do mostly as you do when you start a lisp repl to work
on some project.

The simplest deliver script could be something like this:

```lisp
(in-package :cl-user)

(defun main ()
  (format t "LispWorks delivered application~%"))
  
(deliver #'main "./app" 0)
```

We define a main function to be our application code and call
`delivery` with some parameters we'll explore later. In this case we
are not loading any external application code, just defining a
function.

Now we can ask LW to build this and run the resulting binary

```sh
$ lispworks -build deliver.lisp

...

Shaking stage : Saving image
Build saving image: app
Build saved image: /home/marcecoll/common-lisp/test-deliver/app

Delivery successful - app

$ ./app
LispWorks delivered application
```

This is quite easy. Let's check what those arguments to `deliver`
mean. The first argument, `#'main` in the above script, is the entry
point, the function that will be called when running the resulting
binary. The second one (`"./app"`) is the path for the binary that
will be generated. The third one (`0`) is how hard we want LispWorks
to try to make our image smaller and it's called `level`
internally.. It is a number from 0 to 5, and the higher the level the
more runtime LispWorks removes. It also applies some optimizations
like converting methods into functions in some cases and stuff like
that. Cool!

Let's check what was the size of the generated `app` binary.

```sh
$ ls -lh | grep app
-rwxr-xr-x 1 marcecoll users 47M abr  2 12:18 app
```

47MB is pretty big for a simple print, let's ask LispWorks to reduce
it, let's set the level to 5 and rebuild.

```sh
...
Delivery successful - app

$ ls -lh | grep app
-rwxr-xr-x 1 marcecoll users 7,9M abr  2 12:28 app

$ ./app
Error: Attempt to invoke function *%APPLY-INTERPRETED-FUNCTION* on arguments (#<interpreted function #:NIL 412003FC4C> #:NIL).
*%APPLY-INTERPRETED-FUNCTION* was removed by Delivery.
Check that all functions are compiled before delivery if possible.  If you need to run non-compiled functions after delivery, pass :KEEP-EVAL T to DELIVER.
404001F790 "???"
Quitting
```

Ruh. Roh. What's going on here? By default LispWorks creates functions
as interpreted functions, when tree shaking the image LispWorks also
removed the ability to eval functions since it expects everything to
be compiled.

We can do two things here:

Either add the `:KEEP-EVAL T` to the `deliver` arguments, or add
`(compile 'main)` before the call to `deliver`. In both cases the
image doesn't increase in size too much. It stays in 7,9M, so it
doesn't really matter. It also depends in what your application
uses. If you rely on runtime `eval` then you will want to `:KEEP-EVAL T` 
anyway. If you are loding your code with quicklisp this is usually
handled for you, since quicklisp uses `compile-file`.

This exemplifies a bit more how the delivery system works. It
literally takes the image state as is and depending on the level it
removes as many features as possible to reduce the image size, this
may break some of your code that relies on features that have been
removed. There are very little build checks to make sure the
application will work after very aggressive tree shaking!

Unless you have a particular need to reduce the image further I
recommend starting with level 0 or 1 if that's enough, since it's less
likely some feature you depend on will be removed. If you do need a
higher level, `deliver` has a lot of flags to control what is and what
is not removed, make sure to check those out and test the resulting
binary extensively.

This is unlike any other system I've worked with and I think it's
important to highlight it. Most systems build stage is about code
building and state is usually left for a separate step or at
initialization time. Of course there are ways to do it but they get
hairy quite fast. Because of how image-based builds work, you can
build state at this stage. Anything you do, variables you set, hash
tables you fill, trees you generate will get embedded in the resulting
image.  

This means that precomputing something is not only feasible but easy
to do if needed. To me is a little bit analogous to functions vs
macros, where you can do computation at compile time instead of
runtime. I've done similar things with rust, but it always feels
non-native and hacky, I want to explore a little bit more what can be
done with this in the future.

## A more complex example

This is the deliver script for an application I'm building:

```lisp
(in-package :cl-user)

(require "remote-debugger-client")
(require "tty-inspect")
(load "~/quicklisp/setup.lisp")
(ql:quickload :ami)

;; Load the sites before delivering, since we won't be able to
;; load them afterwards.
(ami.helpers:load-sites)

(multiple-value-bind (sec min hr day mon yr dow dst-p tz)
    (get-decoded-time)
  (let ((commit (uiop:run-program '("git" "rev-parse" "--short" "HEAD")
                                  :output '(:string :stripped t))))
    (setf ami.config:*build* 
          (format nil "~4,'0d-~2,'0d-~2,'0d (~a)" yr mon day commit))))

(lw:deliver #'ami:start-ami "./ami" 0
            :multiprocessing t
            :startup-bitmap-file nil
            :delete-packages '(ql)
            :keep-pretty-printer t)
```

This application is basically a web server that can contain multiple
sites. Let's break it down

```lisp
(require "remote-debugger-client")
(require "tty-inspect")
```

This loads two features of LispWorks that we want for the final image,
the remote debugger (explained in the next section) and `tty-inspect`
to be able to run `(inspect *)` in the application. When run from a
normal listener, inspect automatically loads this feature when
executed. However we cannot do that in the resulting executable since
we will have removed it.

```lisp
(load "~/quicklisp/setup.lisp")
(ql:quickload :ami)
```

Fairly self-explanatory, I setup quicklisp and then use it to load my
system. This makes sure that both my code and all the dependencies
will be availabe in the resulting image.

```lisp
;; Load the sites before delivering, since we won't be able to
;; load them afterwards.
(ami.helpers:load-sites)
```

This is a helper function that goes over everything in the `sites/`
folder and loads it using quicklisp. Making sure that the code for the
sites and all their dependencies are available in the image. This is
needed since `compile-file` is not available at runtime in the
delivered applications.

```lisp
(multiple-value-bind (sec min hr day mon yr dow dst-p tz)
    (get-decoded-time)
  (let ((commit 
          (uiop:run-program '("git" "rev-parse" "--short" "HEAD")
                            :output '(:string :stripped t))))
    (setf ami.config:*build* 
          (format nil 
		      "~4,'0d-~2,'0d-~2,'0d (~a)" 
			  yr mon day commit))))
```

Small code snippet that saves the date and the commit in the
`ami.config:*build*` variable. Here you can see an example of
state building in the image. A very simple one though.

```lisp
(lw:deliver #'ami:start-ami "./ami" 0
            :multiprocessing t
            :delete-packages '(ql)
            :keep-pretty-printer t)
```

Finally calling deliver, apart from the arguments we have already
looked at we've added some keyword arguments to control what is kept
and what isn't.

`:multiprocessing` is off by default unless it's a CAPI
application. As this application uses hunchentoot it needs
multiprocessing to run all the handler threads.

`:delete-packages '(ql)` removes quicklisp form the resulting image
since I don't need runtime quicklisp support. Quicklisp is actually
quite useless at runtime in delivered applications since LispWorks
removes the `compile-file` function.

`:keep-pretty-printer t` is needed for `tty-inspect` to work, since it
needs to pretty print objects.

## Remote Debugging on delivered applications

There is one thing that is pretty bad of delivered applications, they
cannot run slynk/swank servers. The server runs fine but they depend
on `compile-file` and that is unconditionally removed in all delivered
applications (I think it's because they don't want you creating a
competing product to LispWorks using their own tools). Sadly not even
the sly repl works.

Thankfully they do have a solution to at least part of what sly
offers.  You can not change code on the fly on a delivered image, but
you can at least open a listener and a debugger to a delivered
application.

You can setup either the IDE or the application to be the debugger
server, in my case I've setup the application to be the server since
it will be always running and I can connect from somewhere else with
my IDE. Let's see how that works.

You have to first load the `remote-debugger-client` feature in the
deliver script. Then you need to startup the server somewhere in your
initialization tree:

```
(dbg:start-client-remote-debugging-server :announce t)
```

This starts a tcp server at the default port (21102, you can change it
with the `:port` keyword arg).

Then from the IDE you need to `(require "remote-debugger-full")` and
`(dbg:ide-connect-remote-debugging "host" :open-a-listener t)`.

This will connect at the default port on the given `host` and open a
listener window connected to that application.

## Package Troubleshooting

As I find problems delivering some packages I'll add them here as
reference.

### Clack/Lack

Clack/Lack loads middleware dynamically at runtime using quicklisp.
You need to explicitly add the middlewares as dependencies in the asdf
system so the are loaded with the main system. Or else you'll get
quicklisp errors when running the application.

For example:

```lisp
(asdf:defsystem ...
	:depends-on (:lack
	             :lack/middleware/session
	             :lack/middleware/static))
```

It also loads the backend server dynamically, so when you run it on
the repl it's possible everything runs correctly since it will load
`hunchentoot` for you, but you need to depend on it explicitly for a
delivered application.

*The End*
