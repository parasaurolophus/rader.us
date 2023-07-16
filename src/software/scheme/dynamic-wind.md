&copy; Copyright Kirk Rader 2023. All rights reserved.

# dynamic-wind

The built-in `dynamic-wind` procedure is Scheme's equivalent to Common Lisp's
`unwind-protect` or the `try ... finally ...` construct in languages like C#
Java. The name of Scheme's version takes into account that certain code blocks
must be protected not only when the stack is being "unwound" upon exit from a
function but also, thanks to first-class continuations, when the stack is being
"rewound" upon re-entry. Both `unwind-protect` and `try... finally...` (the
latter was modeled on the former) define two sections of code: a section being
protected and a section that is guaranteed to execute when the protected section
is exited, even in case of exit due to non-sequential flow of control. Scheme's
`dynamic-wind` adds a third section that is guaranteed to execute before the
protected section begins, no matter how many times control flows in and out of
it.

```scheme
// before-thunk will execute before protected-thunk
// every time protected-thunk is entered
//
// after-thunk will execute after protected-thunk
// every time protected-thunk exits
//
// dynamic-wind returns whatever is returned by
// protected-thunk each time it exits
(dynamic-wind before-thunk protected-thunk after-thunk)
```

Imagine a program to control a CNC drill press. It might have a function,
`drill-hole`, as the main entry point for &mdash; you guessed it! &mdash;
drilling a hole. Drilling a hole with a mechanical press is a complicated,
multi-step process:

1. The drill must be raised to make it possible to position material under the bit

2. The motor must be stopped for safety before placing, moving or removing
   material under the bit

3. The motor must be started before the bit is lowered

4. The motor should keep running while the bit is being lowered or raised

One can imagine helper functions, called by `drill-hole`, for each of these operations:

```scheme
# naive (unsafe!) implementation of drill-hole
(define (drill-hole)
    (start-motor)
    (lower-bit)
    (raise-bit)
    (stop-motor))
```

As long as everything proceeds as expected, the naive definition implements
the requirements for drilling a hole laid out above. But sometimes unexpected
things can happen. Imagine that the CNC drill press has safety features like a
temperature sensor that should cause the bit to automatically retract if it starts
to overheat. This might be implemented in Scheme by having `lower-bit` return a
continuation such that the operation could be resumed after the drill has cooled
off. To support that, we would need to alter the implementation of
`drill-hole` so that:

- The return value of `drill-hole` is whatever is returned by `lower-bit` (in
  case it is a continuation)

- `raise-bit` and `stop-motor` still get called before `drill-hole` returns, no
  matter what happens during the execution of `lower-bit`

- `start-motor` will always get called before the execution of `lower-bit`
   begins, even if it begins multiple times by way of continuation passing

Here is a complete implementation of a simulation of `drill-hole` that uses
`stdout` to log the simulated operations:

<a id="drill-hole"></a>

```scheme
;; -*- geiser-scheme-implementation: guile -*-

;; display message indicating start-motor was called
(define (start-motor)
  (display 'start-motor)
  (newline))

;; display message indicating stop-motor was called
(define (stop-motor)
  (display 'stop-motor)
  (newline))

;; display message indicating raise-bit was called
(define (raise-bit)
  (display 'raise-bit)
  (newline))

;; simulate lowering the bit while the motor is running
;;
;; along the way, display two overheat exception messages and
;; interrupt the drilling, using continuations to allow resumption
;;
;; return 'hole-drilled
(define (lower-bit)
  ;; get the continuation to use for returning early from this
  ;; procedure
  (call/cc
   (lambda (return)
     ;; display a message indicating the first overheat condition
     (display 'overheating-1)
     (newline)
     ;; return a continuation to allow drilling to resume
     (display (call/cc (lambda (k) (return k))))
     (newline)
     ;; display a message indicating the second overheat condition
     (display 'overheating-2)
     (newline)
     ;; return another continuation to allow drilling to resume
     (display (call/cc (lambda (k) (return k))))
     (newline)
     ;; return 'hole-drilled as the final result
     'hole-drilled)))

;; wrap calls to the complete drilling sequence in dynamic-wind
(define (drill-hole)
  (dynamic-wind
    ;; before
    start-motor
    ;; protected
    lower-bit
    ;; after
    (lambda ()
      ;; use inner dynamic-wind to ensure that
      ;; even if all else fails, the motor will
      ;; be stopped
      (dynamic-wind
        (lambda () #nil)
            raise-bit
            stop-motor))))

;; display message indicating that the drill signaled an overheat
;; exception
(define (wait-for-drill-to-cool)
  (display 'wait-for-drill-to-cool)
  (newline))

;; implement the end-to-end simulated drilling scenario, including
;; exception handling and retries using continuations
(define (simulate-drilling)
  (let ((k (drill-hole)))
    (if (procedure? k)
        (begin
            (wait-for-drill-to-cool)
            (k 'resume))
        k)))
```

Here is the result of invoking `simulate-drilling`:

```
scheme@(guile-user)> (simulate-drilling)
start-motor
overheating-1
raise-bit
stop-motor
wait-for-drill-to-cool
start-motor
resume
overheating-2
raise-bit
stop-motor
wait-for-drill-to-cool
start-motor
resume
raise-bit
stop-motor
$14 = hole-drilled
```

As can be seen from the output, `start-motor` is called before `lower-bit` and
both `raise-bit` and `stop-motor` are called after, each time it is invoked,
whether through normal, sequential flow of control or due to calling
continuations.
