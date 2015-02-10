# R5RS
A simple Scheme(R5RS) interpreter in Haskell(based on 
[this tutorial](http://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf)).
It implements a good enough subset of R5RS to make real programming possible.

*Language Features implemented(Features not in the tutorial are denoted with [x]):*
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

*Language Features soon to be implemented:*
* Local variables
