+++
title = "Eval using the lexical environment in LispWorks"
date = 2024-04-03
author = "Marce Coll"
+++

One of the things that surprised me when I learned Common Lisp was
that `eval` happens in the `nil` lexical environment[^1]. This means
that none of the lexical variables, functions or macros surrounding
the eval are available within the evaluated form.

Take this example:

```lisp
(let ((x 4))
  (eval '(+ x 2)))
  
Error: The variable X is unbound.
```

The reason why that surprised me is that one of the first things that
I tried when I learned about lisp symbolic capabilities was to build a
symbolic
[differentiator](https://en.wikipedia.org/wiki/Derivative). You'd give
it a function, it would symbolically compute the derivative using
the chain rule and then evaluate it. I built a very basic one with
only a couple of rules to test the idea and it seemed to work, the
problem was when I wanted to evaluate the resulting form.

What I attempted was something like this:

```lisp
(defun dx (func x)
  (let ((derivative (differentiate func :over 'x)))
    (eval derivative)))
```

If you try this of course you'll face the same issue we found above.
`Error: The variable X is unbound`. I was confused, I thought symbolic
computation WAS the point of Lisp. At that point I learned about the
dynamic vs lexical environment and I started making sense of what I
was seeing, but I was frustrated and it seemed that what I wanted to
do was impossible.

Indeed, you could do it with a macro, but then you cannot pass it
arbitrary functions at runtime, everything has to be predefined.

Another option was available which was to declare `x` as special[^2],
but then you needed to define some special variables, and the
expressions could only use those. It would work but it was not ideal.

At that point I stopped, frustrated, and learned other parts of the
language. Recently I had a need for something similar again and so I
set to find a solution now that I had some more Common Lisp experience
under my belt.

## `PROGV`

[`PROGV`](http://clhs.lisp.se/Body/s_progv.htm) is a Common Lisp
special form that takes three or more arguments, a list of dynamic
bindings to create, a list of values to set the bindings to and any
number of forms. The forms are evaluated in order with the given 
dynamic bindings set.

```lisp
(defun test-fun ()
  (+ *x* 3))

(progv '(*x*) '(2)
  (test-fun))
  
=> 5
```

With this we can emulate a en eval in a lexical env by setting
everything we have in the lexical environment as a dynamic variable
inside a `PROGV` and then executing the eval. It will behave as if
it was running on the lexenv.

This would be a good moment to see where we are going:

```lisp
(let ((x 4))
  (eval-in-lexenv (+ x 2)))
  
=> 6
```

Which behind the scenes would be transformed into a `PROGV` form:

```lisp
(progv '(x) '(4)
  (eval '(+ x 2)))
```

Now the problem is, `eval-in-lexenv` cannot modify its surroundings,
macros are powerful but not *THAT* powerful[^3]. We could ask the
users to do something like this:

```lisp
(eval-with-let (let ((x 4)) (eval '(+ x 2))))
```

And then build a code walker, check bindings inside, capture some
bindings from outside, etc. But that is very complicated and there are
probably plenty of edge cases and problems, it's far too complicated
and too alien for the user to be worth it in this case.

## Introspecting the lexical environment

`DEFMACRO`'s arglist has a hidden gem that I overlooked the first time
I checked the documentation. With `&environment` we can ask the lisp
compiler to give us the lexical environment[^4] at the macro call site.

For example

```lisp
(defmacro capture-env (&environment env)
  env)
  
(flet ((plus (a b) (+ a b)))
  (let ((x 4))
    (capture-env)))

=> #<Environment 
       venv (#<Venv 275146972128  X>)  
	   fenv ((PLUS . #<COMPILER::FLET-INFO #<interpreted function (SUBFUNCTION (FLET PLUS) :UNKNOWN) 40200038FC>>)) 
	   benv NIL 
	   tenv NIL>
```

It contains variables, functions, blocks and tags in the lexical
environment. But, I'm pretty sure this struct is internal and the
intrface may not be stable. Re-reading the LispWorks documentation
today I found this function:
[`SYSTEM:MAP-ENVIRONMENT`](https://www.lispworks.com/documentation/lw80/lw/lw-sys-93.htm).
With this function you can introspect everything in the lexical
environment you pass as an argument. Let's write a function to get all
variable names in the lexical environment for example.

```lisp
(defun lexical-vars (env)
  "Given an environment object, return all lexical vars"
  (let ((vars nil))
    (system:map-environment env
     :variable
     (lambda (name kind info)
       (declare (ignore info))
       (when (eq kind :lexical)
         (pushnew name vars))))
    vars))
```

Pretty self-explanatory, ask lispworks to map over all the variables,
and then we push them into a list. We filter them by `:LEXICAL` type
because we are also given dynamic variables that have been overwritten
in the current lexical scope, but we don't need to use those since
they are already in the dynamic scope.

Now let's create the *pièce de résistance*, the `eval-in-lexenv`
macro:

```lisp
(defmacro eval-in-lexenv (&body body &environment env)
  (let ((lenv-vars (lexical-vars env)))
     `(progv ',lenv-vars (list ,@lenv-vars)
	    (eval ,(car body)))))
		
(let ((x 2))
  (eval-in-lexenv (+ x 2)))
  
=> 4
```

Short and sweet, we get all the lexical variables, then we pass them
both as names and values to `PROGV`. As names we pass them quoted, so
we get bind the same symbols we have of lexical vars as dynamic
variables, for values we pass them unquoted inside of a `(LIST ...)`
so they get evaluated and capture the values from the surrounding
lexical env, thus setting the dynamic variables to the same values.

There you go, a way to eval in the lexical environment in about 14
lines of easy to understand common lisp code. This code is LispWorks
exclusive tho, but I'm sure there are ways to do it in SBCL, since
`&environment` works there as well. I may add it in the future if
someone sends me a solution that works in SBCL.


## Limitations

This implementation doesn't allow using functions and macros from the
lexical env (as defined by `MACROLET` or `FLET`) although there are
ways around it, like binding the function to a variable and using
`FUNCALL` or `APPLY` inside the eval.

```lisp
(let ((fn (lambda (a b) (+ a b))))
   (eval-in-lexenv (funcall fn 3 4)))
   
; => 7
```

Another thing is that this macro depends on being called at the
lexical environment you want to evaluate it.

If you try to call this macro from within the body of a macro function
you'll be capturing the call environment inside the macro. Of course
if you use it as part of the unevaluated returned form of the macro
everything will work as expected, since that will be evaluated in the
correct lexical environment.

## `SETF` shenanigans

There is a question I have for readers, because there is a bit of a
puzzling behaviour I've seen with this implementation in LispWorks.

When using `eval-in-lexenv`, if inside the evaled expression you
`setf` one of the lexical environment variables, that setf escapes to
the actual lexical environment instead of being contained to the
`PROGV`. Does anyone know why that is the case? Can someone try it in
another implementation?

Example:

```lisp
(let ((x 2))
  (eval-in-lexenv (setf x 8))
  (+ x 2))
  
; => 10
```

This is very confusing to me, because it's something that I wanted and
I was checking how to implement it, but it seems at least in LispWorks
it works by default but I'm not sure why it works, since in there you
should be setting the dynamic variable inside the `PROGV` and not the
lexical variable (since you don't even have access to the lexical one
anyway).


*The End*


[^1]: The nil lexical environment, as the name says has no bindings at all

[^2]: Special and dynamic variables are the same thing, special variables are variables bound in the dynamic environment

[^3]: Recently I learned about a [term rewriting lisp](https://github.com/abuseofnotation/termlisp), which could actually do things like this potentially. I think that particular version is not quite powerful enough for this, but there is no reason it couldn't I think.

[^4]: I think it's not only the lexical environment, but the general environment that overlays the dynamic environment, it also contains special variables overriden in the current lexical environment for example.
