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

```
(open-input-file "foo")
(open-output-file "bar")
```

What you get back from those two functions is an `IO Port`, meaning
the general type is the same. This in turn means that something like this
`(write "foo" (open-input-file "bar"))` is not yielding a type error,
but rather a string representing the IO error.

How can you avoid errors like the above? Firstly, there are a few
check functions:

```
(port? foo)
(input-port? bar)
(output-port? baz)
```

This allows us to do something like this:

```
(if (input-port? foo)
  (read foo)
  #f)
```

Some of such functions are defined in the IO part of the standard library.
Here are the ones I consider the most important:

```
(read? input-file) ; this will either read from the port if it is a input-file
                   ; or return #f.
(write? output-file "some important log or such") ; this is the inverse for an
                                                  ; output file.
```

This should suffice as an appetizer. Let us move on to more specialized topics.

## Input

## Output

## IO in the standard library
