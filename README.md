# zepto
![general version](http://img.shields.io/badge/version-0.6.0-yellow.svg)
![MIT Licensed](http://img.shields.io/badge/license-MIT-blue.svg)
![MIT Licensed](http://img.shields.io/badge/GHC_Version-7.6--7.8-grey.svg)
![Scheme Compliance](http://img.shields.io/badge/R5RS Compliance-Okay-green.svg)
[![Build Status](https://travis-ci.org/hellerve/zepto.png?branch=master)](https://travis-ci.org/hellerve/zepto)

A simple Scheme(R5RS) interpreter in Haskell(based on 
[this tutorial](http://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf),
extended massively).
It implements a good enough subset of R5RS to make real programming possible.
Features implemented include Macros, lazy evaluation, a minimal stdlib, many
native primitives and help for those or for functions provided via docstrings 
included in the function definition. And it actually has a decent shell with
completion and history.

It is very small, so the name might or might not be appropriate.

## Table of Contents

1. **[Maintainers](#maintainers)**
2. **[Installation](#installation)**
3. **[Introduction](#introduction)**
4. **[Libraries](#libraries)**
5. **[Future](#future)**
6. **[Contribute](#contribute)**
7. **[License](#license)**

## Maintainers

* Veit Heller (<veit@veitheller.de>, <veit.heller@htw-berlin.de>)

## Installation

You will need cabal for using zepto. A plain old Makefile is included, too.
Run `make test` to check your installation.

After cloning via git, building via cabal is done via invoking `cabal install`.

## Introduction

If you know Scheme, working in the REPL should be pretty straightforward.
Calling it via `zepto`, you should be greeted by this:

```
zepto Version 0.6.0
Copyright (C) 2015 Veit Heller (GPL)
This is free software; see the accompanying LICENSE for copying conditions.
There is NO warranty whatsoever.

Type 'quit' or press Ctrl-C to exit interpreter
Type 'help' to get a list of commands

zepto> 
```

Now you can just fiddle, maybe try something like

```
zepto> (pow 3 300) ; for schemers: this is a convenience alias for expt
136891479058588375991326027382088315966463695625337436471480190078368997177499076593800
206155688941388250484440597994042813512732765695774566001
```

Please note that integers are promoted when they work together with floats:

```
zepto> (+ 1 1.5)
2.5
```

There are a few datatypes, namely integers, floats, strings, lists and
vectors. Quoted expressions are supported, too.

If you need help with a specific primitive, invoke help on it like so:

```
zepto> (help +)
add two values
zepto> (help "+")
add two values
```

You can also get help for normal functions:

```
zepto> (define (x fst snd) "multiply two values" (* fst snd))
multiply two values; source: (lambda ("fst" "snd") ...)
zepto> (help x)
multiply two values; source: (lambda ("fst" "snd") ...)
```

And it autocompletes your newly created function, too!

Once you're done with the fiddling, just do:

```
zepto> quit

Moriturus te saluto.
```

And you're back to your regular shell.

If you want to see examples of real programs, look in the `examples`
directory.

## Libraries

There are many libraries already imported at startup, such as
two random number generators(one of which is cryptographically
secure) and most of the functions you know from your ordinary
Scheme. There are also a few opt-ins, namely a port of standard
Common Lisp functions (importable via `(load "stdlib/comlist.scm)`)
and a few sort functions (importable via `(load "stdlib/sort.scm")`).

There is currently no complete, comprehensive documentation for those
modules, but you can find out about single commands via aforementioned
`help` primitive.

## Future

Features that are planned, but not yet implemented, include complex numbers, 
hashtables, `call/cc` and a small compiler based on LLVM. 
Both latter features will take a while for me to implement, though. If 
you have any other features, you would like to see in the 
language/implementation, contact me. I'm not an experienced Scheme
programmer myself, so any feedback is welcome.

## Contribute

There is a messy TODO that tells you what could be done if you would like
to contribute. Any contributions are welcome, be it in the form of code,
feature requests or bug reports.

## License

Licensed under GPLv2. Copyright (c) 2014-2015, Veit Heller
