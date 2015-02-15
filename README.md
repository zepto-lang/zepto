# R5RS
![general version](http://img.shields.io/badge/version-0.3.0-yellow.svg)

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
* Value printing via `display`
* `help` statement within REPL providing information to all native primitives
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
* Local variables [x]
* Macros [x]
* Delayed Evaluation [x]

**Language Features soon to be implemented:**
* call/cc
* Foreign Function Interface to C and Haskell

The help message gives a pretty good overview over what's included by now:
```
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
and - and operation
|| - or operation
or - or operation
string=? - compare equality of two strings
string? - compare equality of two strings
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
