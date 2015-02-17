# R5RS
![general version](http://img.shields.io/badge/version-0.4-yellow.svg)
![MIT Licensed](http://img.shields.io/badge/license-MIT-blue.svg)
[![Build Status](https://travis-ci.org/hellerve/R5RS.png?branch=master)](https://travis-ci.org/hellerve/R5RS)

A simple Scheme(R5RS) interpreter in Haskell(based on 
[this tutorial](http://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf)).
It implements a good enough subset of R5RS to make real programming possible.

**Language Features implemented(Features not in the tutorial are denoted with [x]):**
* Infinite precision integers
* Large precision floats(up to ~10\*e310) [x]
* Floats and integers can be used interchangably
* Global variables
* Functions
* Lambdas
* Value printing via `display` [x]
* `help` statement within REPL providing information to all native primitives [x]
* Many native primitives for type conversion and checking
* Tail call elimination
* REPL
  * Loading libraries
  * Command line history [x]
  * Tab completion [x]
* CLI
  * Argument parsing [x]
  * Execution from script files
* stdlib
  * Pairs
  * Utils
  * Math [x]
  * ...
* Local variables [x]
* Macros [x]
* Delayed Evaluation [x]

**Features soon to be implemented:**
* Docstrings
* call/cc
* Foreign Function Interface to C and Haskell
* A Compiler

**Miscellaneous enhancements:**
* Quickcheck testing

The help message gives a pretty good overview over what's included by now:
```
R5RS> help
Primitives:
+ - add two values
- - subtract two values/negate value
* - multiply two values
/ - divide two values
mod - modulo of two values
quotient - quotient of two values
remainder - remainder of two values
= - compare equality of two values
< - compare equality of two values
> - compare equality of two values
/= - compare equality of two values
>= - compare equality of two values
<= - compare equality of two values
&& - and operation
|| - or operation
string=? - compare equality of two strings
string>? - compare equality of two strings
string<? - compare equality of two strings
string<=? - compare equality of two strings
string>=? - compare equality of two strings
newline - print a newline
car - take head of list
cdr - take tail of list
cons - construct list
eq? - check equality
eqv? - check equality
equal? - check equality
pair? - check whether variable is a pair
procedure? - check whether variable is a procedure
number? - check whether variable is a number
integer? - check whether variable is an integer
real? - check whether variable is a real number
list? - check whether variable is list
null? - check whether variable is null
symbol? - check whether variable is symbol
vector? - check whether variable is vector
string? - check whether variable is string
boolean? - check whether variable is boolean
vector - build a new vector
vector-length - get length of vector
string-length - get length of string
make-string - make a new string
make-vector - create a vector
vector->list - makes list from vector
list->vector - makes vector from list
symbol->string - makes string from symbol
string->symbol - makes symbol from string
string->number - makes number from string
string->list - makes list from string
string-copy - copy string
substring - makes substring from string
vector-ref - get element from vector
string-append - append to string

IO Primitives:
apply - apply function
open-input-file - open a file for reading
open-output-file - open a file for writing
close-input-file - close a file opened for reading
close-output-file - close a file opened for writing
read - read from file
write - write to file
read-contents - read contents of file
read-all - read and parse file

Keywords:
apply   - apply function to value
define  - define global variable
error   - print value to stderr
help    - display this help message(use without s-expression)
if      - branch on condition
lambda  - create unnamed function
let     - define local variable
display - print value to stdout
quit    - quit interpreter(use without s-expression)
```
