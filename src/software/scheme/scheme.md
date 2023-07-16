&copy; Copyright Kirk Rader 2023. All rights reserved.

# Scheme

[Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)) is the
ultimate stage in the evolution of the Lisp family of programming languages.
Lisp and Fortran, originally developed at about the same time in the late
1950's, were the first two widely used and studied "high level" computer
programming languages. Prior to their appearance, programming was exclusively
done using assembly language or by entering binary machine code directly using
all those quaint switches, knobs and dials visible on 50's vintage computers.

Lisp and Fortran take fundamentally different approaches to language design.
Fortran was intended to appeal directly to engineers familiar with the branches
of mathematics used in the applied sciences and with the kind of structured
design documentation used in electrical engineering and similar eminently
practical endeavors. The majority of popular programming languages to this day
owe a lot of their syntactical and semantic conventions to Fortran.

Lisp was intended to be as a direct an embodiment of Church's [Lambda
Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) as can be achieved in
the real world. Scheme comes the closest of all Lisp dialects to achieving that
aim. For that reason alone, any programmer who wishes to understand something
about the origin of the very concept of "programming language" would do well to
learn Scheme.

For example, to represent what Church would have a written on a chalk board at
UCLA as \\(\lambda x.+ \hskip0.25em x \hskip0.25em 1\\) (yes, he preferred
prefix notation) as a Scheme expression, one writes `(lambda (x) (+ x 1))` (and,
yes, Lisp syntax introduces a lot of parentheses even though the ability to do
without them is one of the motivations for prefix notation in the first place).
Both expressions represent a function which adds 1 to its argument, \\(x\\). The
first as a mathematical formula and the second as a compilable, executable
snippet of source code. The primary semantic difference between them is that
\\(\lambda x.+ \hskip0.25em x \hskip0.25em 1\\), being a mathematical formula,
has no theoretical limit on the magnitude of \\(x\\) while
`(lambda (x) (+ x 1))` is limited by the magnitude of the `bignum` data type on
a given machine with a given amount of heap space allocated to the process
running the Scheme program.

The following descriptions of particular Scheme features will make the most
sense if you already know at least one Lisp dialect, such as [Common
Lisp](https://en.wikipedia.org/wiki/Common_Lisp). A complete tutorial on Lisp in
general or Scheme in particular is (currently) beyond the scope of these pages.

- [Tail Recursion](tail-recursion.md)
- [Lexical Closures](lexical-closures.md)
- [First Class Continuations](call-cc.md)
- [dynamic-wind](dynamic-wind.md)
- [Engines from Continuations](engines.md)
