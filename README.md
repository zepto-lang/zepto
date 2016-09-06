![zepto banner](https://raw.githubusercontent.com/zepto-lang/logos/master/zepto_logo.png)
![general version](http://img.shields.io/badge/version-0.9.6-yellow.svg)
![GPL Licensed](http://img.shields.io/badge/license-GPLv2-blue.svg)
![GHC Version](http://img.shields.io/badge/GHC_Version-8.0-grey.svg)
[![Build Status](https://travis-ci.org/zepto-lang/zepto.png?branch=master)](https://travis-ci.org/zepto-lang/zepto)

A simple Scheme interpreter in Haskell (originally based on 
[this tutorial](http://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf),
extended massively, some code taken from [husk-scheme](https://github.com/justinethier/husk-scheme)).
It implements a good enough subset of R<sup>5</sup>RS to make real programming possible,
but does not strive for perfect compliance. I guess you could say it tries to
be relaxed enough to not get into the way of other interesting things that may
want to be implemented.
Features included include but are not limited to the full numeric tower (with
extensions even), macros, lazy evaluation, continuations, list comprehensions,
user-defined types, an extensive standard library, many native primitives, a Haskell FFI,
a native Regex datatype and help for all callables functions provided via docstrings included in
their definition.
And it actually has a decent shell with completion and history (written entirely in zepto itself).

It is pretty small, so the name might or might not be appropriate.

## Table of Contents

1. **[Maintainers](#maintainers)**
2. **[Features](#features)**
3. **[Installation](#installation)**
4. **[Introduction](#introduction)**
5. **[Libraries](#libraries)**
6. **[Contribute](#contribute)**
7. **[License](#license)**

## Maintainers

* Veit Heller (<veit@veitheller.de>, <veit.heller@htw-berlin.de>)

## Features

A few of the features were already listed above, but here is a full list of the cool and unusual:
- A full numeric tower with the addition of a hardware integer datatype
- list and hashmap comprehensions
- regex literals
- macros
- lazyness
- continuations
- a modern, awesome package manager (get it [here](https://github.com/hellerve/zeps-system/zeps))
- prototype-based generic programming
- a *huge* standard library, with libraries, for testing, monads, parsing, CLI arguments, dates and time, and much more
- an intuitive Haskell foreign function interface, supported by the package manager
- #lang definitions that are somewhat similar to Racket

There is more, but those are my favorites by far.

## Installation

You will need cabal (the Haskell package manager) for using zepto.

After cloning via git (using the `--recursive` flag to make sure you get all the
submodules), building is done via invoking `cabal install` after pulling the latest
version of all linked submodules.
Or `make`, because a plain old Makefile is included, too. After building you can
run `make test` to check your installation.

A typical installation workflow would look something like this:

```bash
git clone --recursive git://github.com/zepto-lang/zepto.git
cd zepto
cabal update
make
make test
# And then see whether none of the tests yield #f
```

There is also a [Vim plugin](https://github.com/zepto-lang/zepto-vim) 
for all of you terminal hackers (sorry emacs) and an [Atom plugin](https://github.com/hellerve/language-zepto).

## Introduction

**DISCLAIMER:** This is really only a quickstart. If you plan on going
into it a bit deeper, you might want to take a look into the wiki. It all
is a work in progress, though, so the resources might not be as exhaustive
as would be appropriate.

If you know Scheme, working in the REPL should be pretty straightforward.
Calling it via `zepto`, you should be greeted by this:

```
zepto Version 0.9.6, compiled with GHC version 710
Copyright (C) 2015 Veit Heller (GPL)
This is free software; see the accompanying LICENSE for copying conditions.
There is NO warranty whatsoever.
Hail Eris, all rites reversed.

Type ':quit' or press Ctrl-C to exit interpreter,
':help' to get a list of commands or ':license' to get the license text

zepto> 
```

Now you can just fiddle, maybe try something like

```clojure
zepto> (pow 3 300) ; for schemers: this is a convenience alias for expt
=> 136891479058588375991326027382088315966463695625337436471480190078368997177499076593800
206155688941388250484440597994042813512732765695774566001
```

Please note that numerical types are promoted when they work together:

```scheme
zepto> (+ 1 1.5)
=> 2.5
```

There are a few datatypes, namely integers, floats, rationals, exact and imaginary numbers, 
strings, lists, hashmaps and vectors. Quoted and quasi-quoted expressions are supported, too.

A faster version of integers, wrapping natives, is available. Please note that
it is only available via explicitly creating it by invoking `make-small` on a
regular integer. They will wrap around and overflow just as you would expect from
a hardware integer. It string representation is with `s`-suffix. Whenever it comes
in contact with other number types, expect it to be promoted. A quick demo:

```clojure
zepto> (make-small 1)
=> 1s
zepto> (+ (make-small 100) 10) ; Fly away, you're an integer now!
=> 110
zepto> (make-small (pow 2 63)) ; Hardware-dependent
=> -9223372036854775808s
```

*Remember:* If you use small integers, you will have a bad time unless you know
exactly what you are doing! They are unsafe, your mileage may vary.

If you need help with a specific primitive, invoke help on it like so:

```clojure
zepto> (help +)
=> add two values
zepto> (help "+")
=> add two values
```

You can also get help for normal functions:

```clojure
zepto> (define (x fst snd) "multiply two values" (* fst snd))
=> multiply two values; source: (lambda ("fst" "snd") ...)
zepto> (help x)
=> multiply two values; source: (lambda ("fst" "snd") ...)
```

And it autocompletes your newly created function, too!

There are also List and Hash Comprehensions. They look like this:

```clojure
zepto> ; [do-this | for-every <- in, optional-check]
zepto> [(add1 x) | x <- [1 2 3 4]]
=> (2 3 4 5)
zepto> [(+ x 1) | x <- [1 2 3 4], (> x 1)]
=> (3 4 5)
zepto> #{(+ k 1) (* v 1.0) | k v <- #{1 2 3 4}}
=> #{2: 2.0, 4: 4.0, }
zepto> #{(+ k 1) v | k v <- #{1 2 3 4}, (= k 1)}
=> #{2: 2, }
```

The whitespaces matter. The thing is symbol-heavy enough as it is,
so give it at least a bit of space.

Once you're done with the fiddling, just do:

```clojure
zepto> :quit

Moriturus te saluto.
```

And you're back to your regular shell.

You can also run files  or strings by handing them to the program 
like that:

```sh
$ cat t.scm
(display "hi")
$ zepto t.scm
hi
$ zepto --single "(display \"bye\")"
bye
$ cat test_args.scm
(display zepto:args)
$ zepto test_args.scm hi and bye
(hi and bye)
```

As you can see in the last two lines, command line arguments are
available to Scheme via the `zepto:args` variable. It is a list of strings.

If you want to see examples of real programs, look in the `examples`
directory.

## Libraries

There are many libraries already imported at startup, such as
two random number generators(one of which is cryptographically
secure) and most of the functions you know from your ordinary
Scheme. There are also a few opt-ins.

There is currently no complete, comprehensive documentation for those
modules, but you can find out about single commands via aforementioned
`help` primitive.

## Contribute

Any contributions are welcome, be it in the form of code,
feature requests or bug reports. Documentation is especially welcome,
at all times.

## License

Licensed under GPLv2. Copyright (c) 2014-2016, Veit Heller
