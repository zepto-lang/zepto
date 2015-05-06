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

When working with any other number type (even Integers) they are promoted.
Before explaining how the Rational and Complex number types work,
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
(char->integer #\Λ) ; smake integer from char, conforming to ASCII/Unicode; this is a captial lambda, by the way
(string->number "400") ; make number from string, can be any number type except small
```

Those are not a lot, but quite a few and certainly enough to do a few basic
experiments and probably more.

After having those things off the table, let me explain Rationals.
They are actually a pretty smart way to overcome a problem as old
as computers - and I am able to say that it is smart, because it is
smart, effective and not my idea. You have actually seen them before,
namely in above list, but probably not recognized as such.
In Scheme derivatives, a Rational is represented as two numbers
divided by a slash and no spaces, e.g. `1/30000000`. The Rational
is not evaluated to a Float, instead two numbers are kept in memory.
This allows us to represent very numbers precisely instead of cutting
digits off at some point. Cool, isn't it?

The Complex type is a bit peculiar. If you do not know what a complex
number is, you will likely not need this type and even if you do,
you might not have use for it. If you think that is the case, just
skip this section and jump to the next. You're not missing anything
major, except if you ever try to calculate the square root of a negative
number and expect the program to die.
The Complex type is represented as two numbers, also without spaced,
joined by a plus and with a trailing i, like that: `1+1i`. Complex
as well as Rationals exist in many languages, the really killer feature
in zepto in that regard is that those number types are first-class
citizens.

That should be it with the numbers, let's move on to chars and
strings.

## Chars and Strings

The first thing to note is that Chars and Strings are different data
types in zepto, as is convention in Scheme. That means that Strings
are not just an array of characters, but rather a basic building block
in itself. There are ways to let those two types interact, of course,
but let us begin by just introducing the types itself before working
on making them play nicely.

Let's start with Chars.

Chars have a somewhat awkward syntax in Scheme and, for that matter,
zepto. If you want to use a literal character a, you will have to
type `#\a`. This might look weird to you, but at least literal Unicode
characters are supported (an example can be found in `char->integer`
above).

There are a few functions for working with characters:
```clojure
(char-lower-case #\A) ; convert char to lower case
(char-upper-case #\a) ; convert char to upper case
(char=? #\C #\d) ; checks whether chars are equal
(char<? #\B #\Y) ; checks whether one char is less than another
(char>? #\l #\ä) ; checks whether one char is greater than another
(char<=? #\q #\m) ; you know the drill by now
(char>=? #\Ü #\á) ; are we through yet?

; all of the above functions are also available in 
; case insensitive versions:
(char-ci=? #\L #\l)
(char-ci<? #\@ #\\)
(char-ci>? #\X #\#)
(char-ci<=? #\€ #\$) ; if you type #\€ into your repl, you will likely not
                     ; display what you would expect. But it works, promise.
(char-ci>=? #\l #\m)
```

## Lists and Vectors

## Macros

## Continuations

## Atoms
