&copy; Copyright Kirk Rader 2023. All rights reserved.

# Tail Recursion and Loops in Scheme

Consider the traditional definition of the _factorial function_:

\\[
n! =
    \begin{cases}
        1 & \text{if } n \leq 1 \\\\
        n (n - 1)! & \text{otherwise}
    \end{cases}
\\]

This is an example of a _recursive_[<sup>*</sup>](#recursion) function; i.e. a
function that invokes itself in the course of carrying out some calculation. For
example:

\\[
3! = 3 \times 2! = 3 \times 2 \times 1! = 3 \times 2 \times 1 = 6
\\]

If implemented literally as defined above, in
[Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)) this would
be:

```scheme
(define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))
```

The preceding is a mathematically correct representation of the traditional
definition of \\(n!\\) as laid out above, but suffers from the problem that it
only works for fairly small values of \\(n\\). Any moderately experienced
software engineer would be able to explain that the preceding implementation of
`factorial` is subject to a stack overflow error[<sup>**</sup>](#stack). In
other words, `(factorial 3)` will produce `6`, as expected; `(factorial
1000000)` probably will cause a run time error due to too great a recursion
depth. The exact value of `n` that will cause `factorial` to fail will vary
depending on the amount of available stack space allocated to the process
running it at the time it is invoked.

This can be improved through a slight modification to the definition of
`factorial`, involving a helper function:

\\[
    \begin{align*}
        \text{let } n! & = f(n, 1) \\\\
        \text{where } f(x,a) & =
            \begin{cases}
                a & \text{if } x \leq 1 \\\\
                f({x - 1}, {a x}) & \text{otherwise}
            \end{cases}
    \end{align*}
\\]

In this case the implementation helper, \\(f\\), is defined as taking two
parameters. The first parameter, \\(x\\), is the number whose factorial value is
to be computed and is initially the original \\(n\\). The second parameter,
\\(a\\), is the currently accumulated result of the calculation in progress,
initially \\(1\\). The value of \\(f(1, a)\\) is \\(a\\). The value of
\\(f(x, a)\\), while \\(x \gt 1\\), is the result of directly calling \\(f\\)
again, with \\(x - 1\\) and \\(a \times x\\) as parameters.

In _Scheme_ this becomes:

```scheme
(define (factorial n)
    (letrec ((f (lambda (x a)
                    (if (<= x 1)
                        a
                        (f (- x 1) (* a x))))))
        (f n 1)))
```

While these two implementations of `factorial` in _Scheme_ are mathematically
equivalent and both are recursive, the second version that relies on the
two-parameter function `f` is immune from stack overflow. This is because the
_Scheme_ specification requires that "tail calls" not grow the stack. But what
does that mean, exactly?

Note that in the first implementation of `factorial`, the function calls itself
inside an invocation of `*` within the "else" clause of an `if` special form. In
such a case the _Scheme_ compiler, just as that for any programming language,
must arrange to remember the sequence of invocations of `factorial` so that it
can perform the `*` calculation to the value returned by each inner call to
`factorial` before returning from the outer call. The second implementation of
`factorial`, which uses the locally defined helper function `f`, performs the
same mathematical operations arranged in a slightly different sequence. In that
case, both the subtraction in `(- n 1)` and multiplication in `(* a n)` are
fully evaluated _before_ the inner call to `f`. At each invocation of `f`, the
value returned from the outer invocation is simply that which is returned by the
inner invocation without any additional computation. Such an inner invocation is
said to be in "tail position" and it is not necessary to grow the stack when
making the inner call. In effect, the inner call re-uses the existing stack
frame of the outer call without adding a new one of its own. Thus, the only
limit on the value of `n` that exists for this second version of `factorial` is
the maximum range of the `bigint` data type that is the result of the innermost
invocation of `(* a n)`. Another way to look at the difference is that the
tail-call version of `factorial` "remembers" the sequence of calculations using
the accumulated value in the variable `a` so that it does not have to be saved
on the stack. This is an important concept for functional programming generally:
never use an object or stack frame to store state that could more efficiently be
retained in a variable within some closed-over lexical environment.

This special handling of recursion in tail position means that you do not
need[<sup>***</sup>](#loop-constructs) explicit looping constructs like `while`,
`do`, `for` etc. in _Scheme_. The standard idiom for loops in the functional
programming paradigm is to use tail recursion. Scheme facilitates this with
_named `let`_. Here is the preceding example, rewritten more idiomatically using
a named `let`:

```scheme
(define (factorial n)
    (let f ((x n)
            (a 1))
        (if (<= x 1)
            a
            (f (- x 1) (* a x)))))
```

The second and third versions of `factorial` shown above are semantically
identical. They perform the same set of operations, in the same order. The more
compact syntax used in the latter version emphasizes the fact that for _Scheme_,
there is no difference between recursion in tail position and looping. To show
what that means, here is how one might define `factorial` in a language like
_C_, _Java_ etc. without special handling of tail recursion but with a special
`while` loop construct:

```java
int factorial (int n) {

    int a = 1;

    while (n > 1) {

        a *= n--;

    }

    return a;

}
```

The thing to undersand about the latter two _Scheme_ examples and the final
_Java_ example is that all three are semantically equivalent, performing
essentially identical sequences of operations. Anywhere you would use `while`,
`for` etc. in _Java_, _C_ or the like you can use tail recursion in _Scheme_
instead. Further, tail call optimization applies whenever a function is invoked
in tail position, not just cases of loops implemented using functions that call
themselves. For example, [first-class continuations](./call-cc.md) can be
invoked in tail position to implement a true continuation passing style without
recourse to special forms like _trampolines_. But that is a discussion [for
another time...](engines.md).

---

<a id="recursion"></a>

<sup>*</sup> Historically, when mathematicians have talked about "recursive
functions" they meant any case of one function calling another. For a
mathematician, \\(g\\) is being called recursively:

\\[
    \begin{align*}
        \text{let } n & = f(g(x)) \\\\
        \text{where } g & = \lambda y.+ \hskip0.25em y \hskip0.25em 1
    \end{align*}
\\]

Computer programmers have co-opted the term "recursion" to refer only to what a
mathematician would regard as the special case of _self_-recursion. Because
dialects of Lisp like Scheme hearken back to the origin of all programming
languages in the [Lambda Calculus](../philosophy/computability.md), Scheme texts
will often use the terms "call in tail position," "tail call" and "tail
recursion" interchangeably whether or not a function is calling itself, directly
or indirectly.

---

<a id="stack"></a>

<sup>**</sup>The way that function call and return is implemented conventionally
in programming languages targeting [Von Neumann
architecture](https://en.wikipedia.org/wiki/Von_Neumann_architecture) machines
is via a _stack_. A program must have a way to remember the current state of a
computation before invoking a subroutine. It must also be able to restore that
state so that it can continue from where it left off after the subroutine
completes its work. This is done by pushing a _stack frame_ containing a
snapshot of certain critical information about a program's current state as the
top-most entry in a special area of memory, the _stack_. The stack frame
includes information about where the program should resume after the subroutine
completes its work. It then invokes the subroutine and the subroutine consults
the stack frame in order to return to the point from which it was called. The
stack frame is popped off the stack and execution continues. If routine A calls
subroutine B and subroutine B calls subroutine C before returning to A, the
stack must have enough free capacity to store frames for both B and C for the
duration of the execution of C. If the program detects that there is no more
room on the stack at the point at which it is about to call a subroutine, it
signals a "stack overflow" error and the current computation is aborted. Given
the first definition of `factorial` shown above, the stack would need room for
at least `n` frames whenever it is called. Since computer memory is a finite
resource, it does not require enormously large values of `n` to cause a stack
overflow for most real world hardware.

---

<a id="loop-constructs"></a>

<sup>***</sup>Emphasis here on _need_. Early versions of Scheme actually had no
explicit loop constructs but a few were added over time due to popular demand.
The first such, the `do` special form, is modeled on C/C++/Java etc. `for` loops
but uses a syntax that is so tortured that one wonders whether or not the
original authors were trying to make a point and steer people toward tail
recursion based loops or continuation passing by making `do` much harder to use.
In other words, the "popular demand" mostly came from people trying to learn
Scheme and the functional paradigm after having already become very used to
procedural programming using so-called "structured" languages.
