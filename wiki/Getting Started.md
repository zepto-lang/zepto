# Getting Started

This is supposed to be an introduction into zepto.
If you know Scheme already, you can skip to the 
[Atoms](#atoms) section, which is the only addition 
in terms of syntax so far.

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

## The Numerical Tower

Lisp has a famous concept regarding numbers that has found its way
into a lot of different languages, such as Smalltalk. The basic notion
of numbers in zepto is this: there is the most fundamental number type,
which is the integer. You can safely promote it to all kinds of numbers,
because they all sit in higher spots in the tower, but you cannot always
safely demote them without losing precision (1.3 is a float that has no
equivalent integer). You can call `round` and such, but you will lose
precision that way.

So what types are supported?

```
(Small Int->)Integer->Float->Rational->Complex
     10s       100     2.3     4/3      1+1i
```

To illustrate, I have made a little diagram of the tower. At least
integers should be pretty familar to you. They are of arbitrary
precision, which means that you can represent any integer, although
working with them will become *very* slow as they become extremely
big, and any float up until ~1.7e308 (any float bigger than that
is "Infinity"). That is not true arbitrary precision, but it should
suffice.
When a float has hit the fan, it cannot be saved. This essentially 
means that `(- (+ 1.7e308 0.1e308) 0.1e308)` will still yield `Infinity`.

A new concept that zepto introduces is Small Ints, which basically wrap
hardware numbers (so, depending on your machine 32bit or 64bit floats).
That in turn allows for faster number crunching, but also for all the
disadvantages that the hardware brings, namely wrap-arounds. If your
numbers are getting too big and the hardware cannot display them anymore,
they will not behave as expected anymore. Up until now, they can only
be created explicitly by calling `(make-small 1)`, and I intend to keep
it that way. They can be dangerous, so maybe you should refrain from
using them extensively unless you have proven that it will help you
gain a significant speedup. In the REPL, they are displayed with a
trailing `s`, like so:

```clojure
zepto> (make-small 100)
100s
```

When working with any other number type (even integers) they are promoted.

Before explaining how the rational and complex number types work,
let me give you a quick list of standard functions that work with numbers:

````clojure
(+ 1 2) ; add two or more values
(- 1 2 3) ; substract two or more values or negate a single value
(* 1 2 3 4 5) ; multiply two or more values
(/ 1 2) ; divide two or more values
; more on arithmetic operations are to be found in the Math wiki page
(= 1 1.0) ; compare two values
(number? 1) ; check whether arg is a number
(integer? 1.3) ; check whether arg is an integer
(rational? 1/3) ; check whether arg is rational
(real? 1+40i) ; check whether arg is real
(null? 4) ; check whether arg is null; generic for other data types
(exact? 3) ; is number exact
(inexact? 1/40) ; is number inexact
(even? 20+20i) ; is number even
(odd? -20) ; is number odd
(zero? 0) ; is number zero; only for numbers
(positive 10e2) ; is number positive
(negative 10e2) ; is number negative
(complex? 1) ; is number complex
(exact->inexact 1/2) ; make exact number from inexact number
(integer->float 1.5) ; make float from integer
(gcd 1 400) ; greatest common divisor of two numbers
(lcm 1+0i 400) ; least common multiple of two numbers
(abs -19+-12i) ; absolute number of arg
pi ; returns pi as a rational
e ; returns e as a rational
(make-small 40) ; make small from integer
(char->integer #\Λ) ; smake integer from char, conforming to ASCII/Unicode
(string->number "400") ; make number from string, can be any number type except small
```

Those are not a lot, but quite a few and certainly enough to do a few basic
experiments.

## Chars and Strings

## Macros

## Contiunations

## Atoms
