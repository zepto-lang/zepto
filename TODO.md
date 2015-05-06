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
* Many native primitives for type conversion and checking [x]
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
  * Stdlib file completion in strings [x]
* Complex Numbers [x]
* Rational Numbers [x]
* call/cc [x]
* rationals [x]
* Make division by zero not error [x]
* SmallInts [x]
* [] convenience alias for lists
* Add EvalFuncs [x]

**Features soon to be implemented:**

Small:
* Make modulo of Complex work
* fix single number expression (in the repl 1.5lasd is parsed as 1.5)

Medium:
* Profiling shows getting variables(especially namespaced vars) takes ages(~86% of execution time). Fix that.
* Make macros more robust
* Fix continuations
* Make cl libraries work
* Hashtables (SRFI 69)

Big:
* Foreign Function Interface to C and Haskell
* Wrap OpenGL (https://github.com/haskell-opengl/OpenGL)
* A Compiler
  - with backend targetting gpu/ptx (maybe par-define?)
* A solid actor-based concurrency system (+ transactional memory)

**Miscellaneous enhancements:**
* Quickcheck testing
