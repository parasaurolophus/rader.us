&copy; Copyright Kirk Rader 2023. All rights reserved.

# First Class Continuations

As noted in [Tail Recursion in Scheme](scheme-tail-recursion.md), procedure call
and return has traditionally been conceptualized, and often literally
implemented, by storing information that needs to be shared between the caller
and callee in a LIFO stack. In this model, each procedure invocation corresponds
to a LIFO entry, called a _stack frame_, containing the callee's parameters and
the caller's _return address_ &mdash; the point in the code where execution
should continue after the callee exits. Compilers for many programming languages
also create storage for other things like locally defined lexical variables and
a placeholder for a function's return value as part of a procedure's stack
frame.

The idea of a stack frame containing a return address is supported directly by
most CPU instruction sets. The very term, "return address," comes from assembly
language programming. Specifically, most CPU instruction sets include some form
of "jump to subroutine" instruction for pushing onto the stack the memory
address of the next instruction following the "jump to subroutine" while
simultaneously transferring execution to some other address in memory, i.e. the
starting instruction of a subroutine being called. The called procedure exits by
invoking a "return from subroutine" primitive that pops the return address, i.e.
the the memory address of the instruction following the original "jump to
subroutine" instruction, off the stack and jumping there. The net effect is that
execution resumes at the instruction just after the invocation of the subroutine
with the stack restored to the state it had just before the subroutine was
called.

These ideas of a "stack frame" containing a "return address" along with other
aspects of a procedure's invocation such as parameters being passed to it can be
generalized beyond the features supported directly by a CPU's instruction set.
Compiler writers talk about a procedure's _continuation_ rather than its "return
address" and its _execution context_ rather than its "stack frame." Compilers
can, and clever compiler writers do, find ways to detect and exploit certain
special cases to enable CPS (Continuation Passing Style) as an optimization
where practical. But due to the design of most programming languages this is
really just a matter of terminology from the point of view of someone using the
compiler to write code in some programming language. In nearly every popular
language, procedure call and return occurs in patterns dictated by the rules of
a LIFO stack and is subject to "stack depth" limitations requiring the
introduction of lots of special purpose looping constructs and similar
complexities.

Scheme takes a radically different approach. For one thing, Scheme's definition
mandates that the compiler explicitly support one particular feature of CPS,
often referred to as [tail recursion](tail-recursion.md). While compilers for
other languages may or may not implicitly employ tail call optimization in
certain cases, Scheme compilers are required to do so according to explicit,
well-defined rules making it far easier to use safely and correctly when writing
code. Similarly, Scheme mandates that every procedure invocation corresponds to
the creation of a [lexical closure](lexical-closures.md). Among other
significant advantages this completely separates the concerns of a program's
"execution context" from its "continuation." Scheme takes advantage of this
separation of concerns by introducing _first class continuations_ as an
explicitly defined data type. A continuation, in Scheme, is the literal
embodiment, accessible to ordinary Scheme programs, of what for most programming
languages is a very abstract notion only directly accessible to the compiler.

What this means is that Scheme's syntax is very much smaller and much more
consistent than most popular programming languages while supporting features
most other languages either support in only limited ways or completely lack. For
example, there are no looping constructs (`while`, `do`, `for` etc.) in Scheme.
There is no `catch` / `throw` for exceptions. There is not even a `return`
statement. Yet all of these constructs, and far more sophisticated flows of
control, can be implemented easily in Scheme using a combination of just `if`
and `call-with-current-continuation` (usually abbreviated in documentation and
source code as `call/cc`).

Scheme's `if` special form behaves the way you would expect. It executes one of
two branches of code depending on the result of evaluating an expression:

```scheme
;; invoke `do-if-true` if `test` returns a value other than #f,
;; otherwise, invoke `do-otherwise`
(if (test)
    (do-if-true)
    (do-otherwise))
```

While `if` is sufficent for simple branching in otherwise completely sequential
flows of control, `call/cc` enables literally any conceivable non-sequential
flow of control. It is a built-in procedure which takes a procedure as an
argument. That procedure is invoked, passing to it another procedure which
represents the continuation of the flow of control of the original call to
`call/cc`:

```scheme
;; the given lambda is invoked by call/cc
;;
;; k is a procedure which, when invoked,
;; causes the invocation of call/cc to return
(call/cc (lambda (k) ...))
```

As stated in the comments in the preceding example, `call/cc` calls the
procedure it is passed as an argument. That function is passed another procedure,
named `k` in the example, as its parameter. What is special about `k` is that if
it is ever called, the original call to `call/cc` will then return. In addition,
whatever value is passed as a parameter to `k` will become the return value from
`call/cc`.

To see how any of that can be useful, consider the `return` statement built into
many programming languages. Scheme does not define it as a built in procedure or
special form, but it can be easily implemented using `call/cc`:

```scheme
;; writes 1 to stdout followed by a newline
;;
;; returns 42
;;
;; execution never reaches the code that would
;; write 2 to stdout
(define (foo)
    (call/cc
        (lambda (return)
            (display 1)
            (newline)
            (return 42)
            (display 2)
            (newline))))
```

Invoking `foo` will cause 1 to be written to `stdout` while `foo`'s return value
will be 42. Execution of `foo` will not reach the code that would write 2 to
`stdout`. Note that while `call/cc` is pretty magical there is nothing magical
about the name `return`. The parameter name of the anonymous function could have
been anything without changing the behavior of the program.

```scheme
;; writes 1 to stdout followed by a newline
;;
;; returns 42
;;
;; execution never reaches the code that would
;; write 2 to stdout
(define (foo)
    (call/cc
        (lambda (k)
            (display 1)
            (newline)
            (k 42)
            (display 2)
            (newline))))
```

What is significant is that the continuation procedures obtained using `call/cc`
are first-class data objects that can be bound to variables and returned as
values from procedures. Combined with tail call optimization, they make CPS
available as a standard coding idiom in Scheme, available to Scheme programmers
and not just compiler writers, as a natural part of the definition of the
language.

To reiterate for emphasis: most programming languages introduce a number of
distinct syntactic constructs to provide access to CPS features in particular
special cases while Scheme supports CPS directly as a natural idiom, eliminating
the need for a lot of redundant, special-purpose structured programming syntax.
In doing so, it provides the means for programmers to have very fine-grained
control as well as define their own custom flow-of-control constructs.

Given:

```scheme
(define (fn2)
  (call/cc
   (lambda (return)
     (display 1)
     (newline)
     (display (call/cc (lambda (j) (return j))))
     (newline)
     (display 3)
     (newline))))

(define (fn1)
  (let ((k (fn2)))
    (if (procedure? k)
        (k 2))))
```

Invoking `fn1` will display:

```
> (fn1)
1
2
3
```

This is because:

1. `fn1` calls `fn2`, binding what it returns to the variable `k`
2. `fn2` creates a continuation for itself named `return`
3. `fn2` writes 1 followed by a newline to `stdout`
4. `fn2` creates a continuation for the point in its flow it has reached,
   binding it to `j`
5. `fn2` uses its `return` continuation to pass the continuation `j` to its caller
6. the combination of the preceding steps means that `k` in `fn1` is bound to
   `j` from `fn2` when execution first returns to it
7. `fn1` tests `k` to see if it is a procedure
8. since `k` is a continuation, `procedure?` returns `#t` and so `fn1` invokes
   it, passing 2 as a parameter
9. passing 2 to the continuation in `k` causes the `call/cc` that created it in
  `fn2` to return with the value 2 (since that is what was passed to it from `fn1`)
10. `fn2` writes 2 (the value received from `fn1`) followed by a newline to `stdout`
11. `fn2` writes 3 followed by a newline to `stdout` and returns whatever the
    `newline` procedure returned as its own value
12. the invocation of `fn2` returns to `fn1` _a second time_, this time with the
    final result returned from `fn2`'s invocation of `newline`
13. since `newline` does not return a procedure, `fn1` simply exits without
    taking any further action

In other words, `fn1` and `fn2` demonstrate how first-class continuations can be
used to implement similar functionality to the `return`, `catch` and `throw`
constructs of other languages. Further, they show that in Scheme "exceptions"
are easily resumable, in ways that many other programming languages do not
support. This is just an _amuse bouche_ sized taste of what can be achieved
using Scheme continuations.

There are a number of things to note about the preceding:

- _Continuations can be used for two-way communication between execution contexts_

  Just as `fn1` receives data as return values from `fn2`, `fn2` receives
  data as the parameter to its inner continuation when it is invoked by `fn1`.

- _A given contination can be invoked from multiple points in a flow_

  See the [scan-sample](dynamic-wind.md#scan-sample) example, which is a more
  elaborate version of `fn1` / `fn2`

- _Multiple continuations can exist for different points in the flow through an
  execution context_

  Both `return` and `j` are continuations for `fn2`'s execution context, but at
  different points within its body. Care must be taken to use the correct
  continuation to reach the desired point a non-sequential flow of control.

- _Using continuations often ends up looking like a loop of some kind_

  Here, the same call to `fn2` exits twice, invoking the body of `fn1` multiple times
  even though the code as written appears at first glance completely sequential.
  This is a standard pattern to which you should get used, and which you can exploit
  to good advantage.

The "hello world" level of examples here only scratch the surface of what is
possible with first-class continuations. See [Engines from
Continuations](engines.md) for an extended example of how a particular style of
multitasking can be implemented using continuations.

The non-sequential flows of control enabled by continuations raise the same
kinds of concerns as those for which Common Lisp's `unwind-protect` exists,
along with the `try... finally...` construct available in many languages. For
Scheme the stakes are even higher since, as demonstrated above, continuations
are not a "one way trip" out of an excecution context like `return` or `throw`.
Continuations can also be used to re-enter a previously exited context. This the
purpose of [dynamic-wind](dynamic-wind.md) is semantically similar to
`unwind-protect`, except with support for running code both before and after the
protected code every time the protected code is entered or exited. See
[scan-sample](dynamic-wind.md#scan-sample) for a much further elaborated
exception handling example demonstrating how the relationship between `fn1` and
`fn2` shown above can be extended to handle something closer to a real world use
case.
