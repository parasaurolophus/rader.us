&copy; Copyright Kirk Rader 2023. All rights reserved.

# Engines from Continuations

A practical application of continuations to implement sophisticated flows of
control.

- [engines.ss](engines.ss)

In <a href="ftp://www.cs.indiana.edu/pub/techreports/TR254.pdf">Engines from
Continuations</a>, Dybvig and Hieb [1988] describe how the flow of control
abstraction known as "engines" can be implemented using Scheme's first-class
continuations. The following source code is adapted from one of the versions of
`make-engine` described by Dybvig and Hieb.

In this implementation, an engine is simply a Scheme procedure of three
arguments. For example, the following code will invoke the procedure named
`thunk` as an engine:

```scheme
(letrec ((return
            (lambda (value remaining-ticks)
                (display "engine returned ")
                (display value)
                (newline)))
            (expire
            (lambda (new-engine)
                (display "engine expired, ")
                (display "use the given new ")
                (display "engine to resume")
                (newline)))
            (thunk
            (lambda ()
                (decrement-timer)
                (display "engine running")
                (newline))))
    ((make-engine thunk) 1 return expire))
```

This is different from simply calling `thunk` directly in that it will be given
a finite amount of "fuel" in the form of "timer ticks." If `thunk` completes
before the engine runs out of fuel, it will call `return`. If the fuel runs out
before `thunk` completes, `expire` will be invoked with a new engine that can be
used to resume the interrupted computation in `thunk`.

_See the warnings in the source code comments regarding the use of
`decrement-timer` in procedures passed to `make-engine`._

This implementation differs from that in the original paper in a few ways, all
thoroughly documented in comments in the source code. Of particular interest:

- Eliminates the need for the `make-simple-engine` helper function defined and
  used in the paper

  Specifically, `make-engine` wraps the invocation of the procedure it was
  passed in a call to `engine-return`

- Adds `engine-expire` for symmetry with `engine-return`

  Calling `engine-expire` causes the currently executing engine to
  pass control to its expiration handler immediately in the same way
  that calling `engine-return` causes an immediate invocation of the
  return handler.
