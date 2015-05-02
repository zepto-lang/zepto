# IO in zepto

Doing IO in zepto is pretty straightforward (or so I hope), but
there are a few pitfalls. This tutorial aims to provide you
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

```scheme
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

```scheme
(port? foo)
(input-port? bar)
(output-port? baz)
```

This allows us to do something like this:

```scheme
(if (input-port? foo)
  (read foo)
  #f)
```

Some of such functions are defined in the IO part of the standard library.
Here are the ones I consider the most important:

```scheme
(read? input-file) ; this will either read from the port if it is a input-file
                   ; or return #f.
(write? output-file "some important log or such") ; this is the inverse for an
                                                  ; output file.
```

This should suffice as an appetizer. Let us move on to more specialized topics.

## Input

With all the initial hurdles out of the way, let us do some input. There are a
few functions used for doing input, here is an overview:

```scheme
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

## IO in the standard library
