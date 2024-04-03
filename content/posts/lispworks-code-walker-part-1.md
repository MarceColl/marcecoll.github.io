+++
title = "Lisp Codewalker: Basic Codewalker (part 1)"
date = 2024-04-03
author = "Marce Coll"
draft = true
+++

In this series we are gonna implement a simple lisp codewalker on
LispWorks and while doing so we'll learn a bit more about Lisp and
Lispworks. A codewalker is basically a piece of code that understands
lisp at a higher level than just simple S-Expr. While lispers like to
think that the code they write is just lists, the truth is that in
order to evaluate it we need more information than that. Both the
dynamic environment and the lexical environment need to be accessed in
order to make sense of what something does.

In order to build more complex and advanced macros some amount of code
walking is necessary. For example, let's say we want to create a macro
that renames a variable. A trivial implementation could look something
like this.

```lisp
(defmacro rename-var ((var-to-rename new-name) body)
  (mapcar
   #'(lambda (form)
       (cond
        ((listp form) 
          `(rename-var (,var-to-rename ,new-name) ,form))
        ((and (symbolp form) 
          (eq form var-to-rename)) 
         new-name)
        (t form)))
   body))
```

Now if we expand a call to this macro

```lisp
(rename-var (a b) (+ a 3))

(+ B 3)
```

We replaced `'A` by `'B`, exactly like we wanted right? This wasn't so
hard! Let's use it in more places!

```lisp
(rename-var (a b) 
	(if (a 'a 4) a 12))
	
(IF (B 'B 2) B 3)
```

Oops, it also replaced a symbol and a function name, we never wanted
that. I hope now it's clear why when rewriting expressions we need
to know what those expressions mean and treat the S-Expressions in
context.

## Evaluation

Let's start with a simple model of evaluation:

 + (<func> &args) calls function with args, function is defined in the
   dynamic envrionment.
 + If a symbol is found in the cdr of a list it's considered a variable.
 + If a symbol is found in the car of a list it's considered a function.
 + There is no quoting or macros. There are no special forms.
 
Let's write our first codewalker implementation with this in mind.

We'll start by writing a function `walker` that takes two arguments, a
function that will take two arguments `(type value)`, where type will
either be `:function` or `:variable` depending on the symbol meaning
and value will be the symbol. The second argument will be a form to
walk.

```lisp
(defun walker (fn form)
  (flet ((apply-walker-function (v)
           (cond
            ((listp v) (walk fn v))
            ((symbolp v) (funcall fn :variable v))
            (t v))))
    (let ((func-result (funcall fn :function (car form)))
          (cdr-results (mapcar #'apply-walker-function (cdr form))))
      `(,func-result ,@cdr-results))))
```

Let's test it out

```lisp
(walker 
  (lambda (type val) 
	(if (and (eq type :variable) (eq val 'b)) 
		'z 
		val))
  '(b a b c 2))
  
=> (B A Z C 2)
```

We write a transformation function that when the given value is of
type `:variable` and the symbol is `B`, then it changes it to `Z`. As
you can see the first `B` in the form is not transformed, since it is
a function. We could transform it for example like this.

```lisp
(walker 
  (lambda (type val) 
    (cond
	 ((and (eq type :variable) (eq val 'b)) 'z)
	 ((and (eq type :function) (eq val 'b)) 'other-function)
	 (t val)))
  '(b a b c 2))
  
=> (OTHER-FUNCTION A Z C 2)
```

Now we can rewrite functions and variables independently! Let's add
support for quoted symbols.

Adding support to quoted symbols is as easy as checking if the
function is `QUOTE` for a given list then we pass a new type to the
transform function, the `:symbol`. We are not handling quoted
expressions yet.


