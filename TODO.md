# TODO

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
* Docstrings [x]
* Numbers with e-notation [x]
* Quasiquoting [x]
* Completion and History [x]
* Complex Numbers [x]
* call/cc [x]

**Features soon to be implemented:**
* Profiling shows getting variables(especially namespaced vars) takes ages(~86% of execution time). Fix that.
* Make cl libraries work
* Hashtables (SRFI 69)
* Foreign Function Interface to C and Haskell
* Wrap OpenGL
* A Compiler

**Miscellaneous enhancements:**
* Quickcheck testing
