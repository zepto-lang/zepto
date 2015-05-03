# Getting Started

This is supposed to be an introduction into zepto.
If you know Scheme already, you can skip to the 
[Atoms](#atoms) section, which is the only addition 
in terms of syntax yet.

## Basic syntax

The syntax of Lisp derivatives is incredibly simple.
It is infamous for having a lot of brackets, but that
is for a reason. Expressions are not grouped by binding
power, like in most other languages, but instead in 
brackets. Inside a bracket, there has to be a single
statement, such as:

````clojure
(+ 1 1)
```

Note that everx operator is a function in Scheme (and thus,
zepto), so it is always written in prefix notation as opposed
to the infix notation we are used to.

Let's see this in action on a more complex example:

```clojure
(define (abs x) "returns the absolute of x"
  (if (>= x 0)
    x
    (* x -1)))
```

That is a lot of information all at once. Let us step through
it. The `define` keyword binds the first argument to the second
argument, e.g. in `(define x 10)`. It is also used to define functions.
The name of the function we define is abs and it takes a single argument
x. We also gave define an optional docstring so that when we ever forget
what abs is, we can just do `(help abs)` (for Clojure users: there is also
a convenience alias called `doc`), which will print the docstring together
with the function signature. After that comes the function body. Functions
are always a single instruction, except when they are grouped by `begin`,
which we will discuss later. Here we check whether the number is bigger
than 0 and if so, return the number (that is not an operation, so it does
not need brackets). If it is not, we are multiplying it with -1 in order
to make it positive. That does need brackets, because it is an instruction.

Bear with me so far? Good. Let us move on then.

## The Number Tower

## Macros

## Contiunations

## Atoms
