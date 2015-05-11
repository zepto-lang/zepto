# Getting Started

This is supposed to be an introduction into zepto.
If you know Scheme already, you can skip to the 
[Atoms](#atoms--symbols) section, which is the only addition 
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

Note that every operator is a function in Scheme (and thus,
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

## Atoms & Symbols

There is a syntactic addition to zepto that may confuse Scheme, Lisp and Clojure
programmers, albeit for different reasons. But I am getting
ahead of myself. First of all, zepto has a basic notion of symbols like any
other language in the Lisp realm. This allows for constructs such as
`(define x 10)` and the like; in other words, values can be bound to names.

This is wonderful, but there is a piece of syntactic sugar that is elementary
to zepto, but not necessarily to other Scheme interpreters, which is Atoms.
Now, before any of you Clojure guys jump to conclusions, it is not what you
call Atoms. In Clojure and Common Lisp land, this concept is known as Keywords.
I simply decided to use them Atoms, because Keywords is a terrible name for that
concept in my opinion. But that is just my personal opinion and you are free to
disagree with me.

For all of you not familiar with what I just ranted about, Atoms are simply symbols
with an inherent value. They stand for themselves, i.e. `:ok` is just that, it is
`:ok`. You also cannot assign something to them, as in `(define :ok "ok")`.
So why not just use `"ok"` then? Fair enough, that would be possible, but
unelegant and, in zepto, unidiomatic. If you do not feel like arguing with me on
that - and you really should! Don't just drink the Kool-Aid. -, just keep in mind
that strings are mostly used when we need a textual representation of something.
If we need a programmatic representation of some higher level concept, use Atoms.
Let me give you a slightly silly example for when to use Atoms:

```clojure
(define (run f) "runs f and wraps booleans in atoms"
  (if (f)
    :ok
    :error))
```

We could now do something like `(run (lambda () #t))`, which would yield `:ok`.

I hope you can see by now that using Atoms looks a bit cleaner than passing around
strings all the time. There are a few functions that check for common Atoms:

```clojure
(ok? :ok)
(error? :error)
(yes? :yes)
(no? :no)
```

There is a little caveat on the whole "Atoms remain unevaluated" thing. As you may
have already noticed by now, REPL keywords (such as `quit`) share their syntax with
Atoms. They are not the same thing, but they look the same. So typing keywords in the
REPL can differ from a scripts' behaviour (where you have to us `(exit)` or `(quit)`
to end a script explicitly. There is an ongoing discussion (within myself) whether I
should replace one or the other with `#`, but I have not settled yet.

*For Clojure people:* Other than in Clojure, using Atoms in zepto does not result
in a significant speedup in zepto. They are just a different syntactic concept to
keep your code clean, not a funky data structure. In fact, a bit of profiling shows
they might even be (very slightly) slower than Strings at the moment. That will
change eventually but do not expect them to be blazingly fast any time soon.

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
(char? 1) ; check whether arg is char
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

Chars are normally not all that interesting by themselves, though,
but rather when they're joined together, namely as strings. As I
already told you, Strings in zepto are a datatype that is seperate
from characters which means that they are more than just
`'(#\l #\i #\s #\t #\s)`. This allows for greater flexibility.
Many standard functions behave a lot as if they were, though.
There are also a few functions for working with Strings in zepto,
namely:

```clojure
(string? #\a) ; checks whether arg is string
(string=? #\a) ; checks wether strings are equal
(string<? #\a) ; you already know all those functions
(string>? #\a)
(string<=? #\a)
(string>=? #\a)
(string-ci=? #\a)
(string-ci<? #\a)
(string-ci>? #\a)
(string-ci<=? #\a)
(string-ci>=? #\a)
(string '(#\w #\o #\w)) ; the string "constructor"; takes one or more characters
(make-string 10) ; another string constructor. it takes the initial length as integer
(string-length "doge") ; gets length of string
(symbol->string duck) ; creates a string from symbol
(list->string [#s]) ; behaves as the constructor, but takes only lists
(string-copy "lol") ; returns a copy of a string
(substring "zepto" 1 3) ; returns a substring delimite by two integers
(string-ref "lisp" 1) ; returns a reference to a string element (as char)
(string-find "scheme" #\e) ; returns first occurrence of the element as index
(string-append "a" "b") ; appends char or string to string
```

The purists among you may notice some deviations from R5RS.
Most of it is by design, although if you think some of this is
unacceptable behaviour, create a pull request of file an issue,
I am always open to suggestions.

Programmatically, strings are not used as often. [Atoms](#atoms--symbols) are
used more frequently.

This concludes our short introudction to Strings in zepto.
Next up are Lists and Vectors.

## Lists and Vectors

I lied. This section is not only about Lists and Vectors, but also about
Quasiquoted Lists and Dotted Lists, two data structures which behave similarish
to Lists, but are *not* the same.

Let's talk about Lists. Lists are heterogenous collections. They can be used
like arrays in other languages, but they are actually much more than that.
You can also see them as unevaluated code, really, e.g. `'(+ 1 2 3)` is a valid
List. The little `'` is the symbol that means that the following brackets are
not code, but data. You can read this as `quote`, which makes sense - at least 
to me. If you come from a programming language like Python or Ruby,
you might find the alternative square bracket notation - you can write the above
list as `[+ 1 2 3]` - rather convenient. The quoted expression or list can later
be transformed into code by calling eval on it, i.e. `(eval '(+ 1 2 3))` will
return `6`.

Lists are not your run-of-the-mill arrays then. You can use them as such, but
that can be somewhat clumsy. If you want to have high-performance, plain-old-data
collections, I'd suggest you go for Vectors. Before explaining the concept of Vectors,
I will again supply you with a list of functions for working with Lists:

```clojure
(list? [foo bar baz]) ; is arg a list
(list 1 2 3) ; create list from elements
(head [1 2 3]) ; get head (first element) of list
(tail [1 2 3]) ; get tail of list
(indexed-tail [1 2 3] 2) ; get tail of list beginning at index
(list-tail [1 2 3] 2) ; alias for tail
(cons 1) ; construct list
(car [+ 1 2]) ; alias for head
(cdr [+ 1 2]) ; alias for tail
(cadr [+ 1 2]) ; get head of tail
(cdar ...)
(cddr ....)
[...]
(cddddr ...) ; this is the longest version of it in the stdlib
(list-ref [+ 1 2 3] 2) ; get element from list at index
(vector->list #(1 2 3)) ; make list from vector
(string->list "hi") ; makes list from string
(list-append '(a) "b") ; appends element or list to list
```

Let's talk about Vectors for a brief moment. Vectors are optimized
for random access. They are fast, they are small. But they are also
less cool than list, because they are only data and cannot become code,
except through converting it to a list before. As such, you can use them
wherever you want to efficiently manipulate data, but nothing too meta.
As an aside: there is an experimental matrix library in the standard
library that wraps vectors of vectors and makes working with such data
easier; if you ever find yourself in a position where you have to do matrix
manipulation, feel free to have a look at it. There is a section on matrices
in the [Math](github.com/hellerve/zepto/wiki/Math) chapter.

## Macros

## Continuations

