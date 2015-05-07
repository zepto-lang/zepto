# IO in zepto

Doing IO in zepto is pretty straightforward (or so I hope), but
[there are a few pitfalls](#output). This tutorial aims to provide you
with all knowledge that is necessary to print things to stdout
and into files.

## Introductory Terms

There is one core concept in zepto that is connected to IO, namely
ports. So what are ports?

Ports are anything you can read from and write into. That includes
files, of course, but also such things as the standard input and
output devices, commonly known as `stdin` and `stdin`. Those are actually
pretty similar to the `FILE` type in C or the `file` objects in Python.

You can create them in two ways, depending on their type. There are
two ways of opening files, namely:

```clojure
(open-input-file "foo")
(open-output-file "bar")
(close-input-file input-file)
(close-output-file output-file)
```

What you get back from the two open functions is an `IO Port`, meaning
the general type is the same. This in turn means that something like this
`(write "foo" (open-input-file "bar"))` is not yielding a type error,
but rather a string representing the IO error. Closing is done by invoking
the analogous call.

How can you avoid errors like the above? Firstly, there are a few
check functions:

```clojure
(port? foo)
(input-port? bar)
(output-port? baz)
```

This allows us to do something like this:

```clojure
(if (input-port? foo)
  (read foo)
  #f)
```

Some of such functions are defined in the IO part of the standard library.
Here are the ones I consider the most important:

```clojure
(read? input-file) ; this will either read from the port if it is a input-file
                   ; or return #f.
(write? output-file "some important log or such") ; this is the inverse for an
                                                  ; output file.
```

This should suffice as an appetizer. Let us move on to more specialized topics.

## Input

With all the initial hurdles out of the way, let us do some input. There are a
few functions used for doing input, here is an overview:

```clojure
(read)
(read :stdin)
(read (open-input-file "foo"))
(read? (open-input-file "foo"))
(read-contents "foo")
(parse "foo")
```

We already covered `read?` and parts of `read`. The other two calls to read
will both read something from stdin, namely until the first whitespace is
encountered. This behaviour can be found in `(read input-file)`, too.

The last two calls are a bit special. `read-contents` is a convenience function
allowing you to pass the filename as a string that will give you back the file
contents as a string. `parse` does exactly the same, although it will also parse
the file, which means that it will have to be a valid scheme file. That is for
all of you metaprogramming fiends.

## Output

Output is pretty similar to Input, only the other way around. Makes sense,
doesn't it? The relevant function calls are:

```clojure
(write)
(write "bla" :stdout)
(error)
(write "bla" :stderr)
(write (open-input-file "foo"))
(write? (open-input-file "foo"))
(newline)
(color :black)
(escape-sequence 30)
```

It all behaves pretty similarly to the Input functions. If you want to
write to stderr, either call `error` or `write` and pass in the escaped
atom of `stderr`. Calling color will change your outputs color. If you
are done with the funkiness, call `color` again, this time either with
no arguments or, if you want to make it more verbose `reset` or `none`.
`escape-sequence` is a bit more general. You can provide it an integer
and it will wrap a ANSI escape sequence around it. The above example will
colorize your prompt red, for instance.

**There is one major pitfall here:** Ports are buffered, so depending
on how large the chunks you write are you might have to close it before
expecting anything in the file. *The streams are not guaranteed to be
written when the REPL or the program ends unless you explicitly close
the port.* This does not apply to stdout, of course.

## IO in the standard library

There are not as many standard library abstractions to IO as I would
hope for yet. However, there are a few wrapper functions. We already
saw two of them, namely `read?` and `write?`. The other ones are: 

````clojure
(call-with-input-file "foo" some-func)
(call-with-output-file "bar" some-func)
(with-input-from-file "baz" some-func)
(with-output-to-file "fooz?" some-func)
```

*None of those are extensively tested, though*, so I would advise
against using them at the moment.
