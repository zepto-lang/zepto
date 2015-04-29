# zepto
![general version](http://img.shields.io/badge/version-0.6.10-yellow.svg)
![GPL Licensed](http://img.shields.io/badge/license-GPLv2-blue.svg)
![GHC Version](http://img.shields.io/badge/GHC_Version-7.4--7.10-grey.svg)
![Scheme Compliance](http://img.shields.io/badge/R5RS Compliance-Okay-green.svg)
[![Build Status](https://travis-ci.org/hellerve/zepto.png?branch=master)](https://travis-ci.org/hellerve/zepto)

A simple Scheme(R5RS) interpreter in Haskell(based on 
[this tutorial](http://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf),
extended massively, some code taken from [husk-scheme](https://github.com/justinethier/husk-scheme)).
It implements a good enough subset of R5RS to make real programming possible.
Features implemented include the full numeric tower, macros, lazy evaluation, 
a minimal stdlib, many native primitives and help for those or for functions 
provided via docstrings included in the function definition. And it actually 
has a decent shell with completion and history.

It is very small, so the name might or might not be appropriate.

## Table of Contents

1. **[Maintainers](#maintainers)**
2. **[Installation](#installation)**
3. **[Introduction](#introduction)**
4. **[Customizing the REPL](#customizing-the-repl)**
5. **[Libraries](#libraries)**
6. **[Why](#why)**
7. **[Future](#future)**
8. **[Contribute](#contribute)**
9. **[License](#license)**

## Maintainers

* Veit Heller (<veit@veitheller.de>, <veit.heller@htw-berlin.de>)

## Installation

You will need cabal for using zepto.

After cloning via git, building is done via invoking `cabal install`.
Or `make`, because a plain old Makefile is included, too. After building you can
run `make test` to check your installation.

## Introduction

If you know Scheme, working in the REPL should be pretty straightforward.
Calling it via `zepto`, you should be greeted by this:

```
zepto Version 0.6.10
Copyright (C) 2015 Veit Heller (GPL)
This is free software; see the accompanying LICENSE for copying conditions.
There is NO warranty whatsoever.
Hail Eris, all rites reversed.

Type ':quit' or press Ctrl-C to exit interpreter,
':help' to get a list of commands or ':license' to get the license text

zepto> 
```

Now you can just fiddle, maybe try something like

```
zepto> (pow 3 300) ; for schemers: this is a convenience alias for expt
136891479058588375991326027382088315966463695625337436471480190078368997177499076593800
206155688941388250484440597994042813512732765695774566001
```

Please note that numerical types are promoted when they work together:

```
zepto> (+ 1 1.5)
2.5
```

There are a few datatypes, namely integers, floats, exact and imaginary numbers, 
strings, lists and vectors. Quoted and quasi-quoted expressions are supported, too.

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
zepto> :quit

Moriturus te saluto.
```

And you're back to your regular shell.

You can also run files  or strings by handing them to the program 
like that:

```
$ cat test.scm
(display "hi")
$ zepto test.scm
hi
$ zepto --single "(display \"bye\")"
bye
$ cat test_args.scm
(display args)
$ zepto test_args.scm hi and bye
(hi and bye)
```

As you can see in the last two lines, command line arguments are 
available to Scheme via the `args` variable. It is a list of strings.

If you want to see examples of real programs, look in the `examples`
directory.

## Customizing the REPL

If you are like me, you want to make your Interpreter your own.
Well, you can, sort of. There are exactly three meta commands for changing
the prompt as of right now, namely `:prompt`, `:prompt-color` and 
`:prompt-toggle-space`, which let you customize your prompt. A quick demo:

```
zepto> :prompt-toggle-space
zepto>:prompt doge>
doge>:prompt-toggle-space
doge> doge
wow such fancy
```

If you want to use spaces in your prompt, do it like this:

```
zepto> :prompt "i am a spaced prompt$ "
i am a spaced prompt$ "yay"
yay
```

An example of using `prompt-color` is not really feasible here, but trust me when
I say that it will colorize your prompt if you give it a string denoting the color.
Try it out by typing `:prompt-color green`.

If you do not like being greeted by a bloated header every time you start the REPL,
invoke it with the `-S/--silent` option.

I will implement other features and config file support as we go along.

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

## Why

Nowadays you need a good excuse to build another implementation of Lisp;
everyone has one up their sleeve and except for learning, there is not 
much you can gain from it - or is there?

Up until now, zepto is nothing too fancy. When everything will go according
to my plans, however, it will evolve into a dialect of Scheme tailored towards
Multimedia (Audio, Video), separately optimizing functions concerned with
audio and video programming through special primitives.

Until I can do that, however, I want to implement a good (superset|subset|dialect) 
of R5RS (and|or) Common Lisp.

## Future

Features that are planned, but not yet implemented, include hashtables, 
proper `call/cc`, better macros and a small compiler based on LLVM. 
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
