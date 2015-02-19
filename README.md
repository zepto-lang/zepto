# R5RS
![general version](http://img.shields.io/badge/version-0.4.0-yellow.svg)
![MIT Licensed](http://img.shields.io/badge/license-MIT-blue.svg)
[![Build Status](https://travis-ci.org/hellerve/R5RS.png?branch=master)](https://travis-ci.org/hellerve/R5RS)

A simple Scheme(R5RS) interpreter in Haskell(based on 
[this tutorial](http://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf),
extended massively).
It implements a good enough subset of R5RS to make real programming possible.
Features implemented include Macros, lazy evaluation, a minimal stdlib, many
native primitives and help for those.

## Installation

You will need cabal/ghc for using R5RS. If you do not have cabal or do not
want to install R5RS, a plain old Makefile is included, too.

After cloning via git, building via cabal is done via invoking `cabal install`.
You can also do it via invoking `make`, which will build R5RS locally(the executable
can then be found in the `bin` directory).

## Introduction

If you know Scheme, working in the REPL should be pretty straightforward.
Calling it via `r5rs` (`bin/r5rs` if you just built locally), you should
be greeted by this:

```
R5RS Version 0.4.0
Type 'quit' or press Ctrl-C to exit interpreter
Type 'help' to get a simple help message

R5RS>
```

Now you can just fiddle, maybe try something like

```
R5RS> (pow 3 300)
136891479058588375991326027382088315966463695625337436471480190078368997177499076593800
206155688941388250484440597994042813512732765695774566001
```

If you need help with a specific primitive, invoke help on it like so:

```
R5RS> (help +)
add two values
R5RS> (help "+")
add two values
```

Once you're done with the fiddling, just do:

```
R5RS> quit

Moriturus te saluto.
```

And you're back to your regular shell.

