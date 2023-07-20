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
;; before-thunk will execute before protected-thunk
;; every time protected-thunk's body is entered
;;
;; after-thunk will execute after protected-thunk
;; every time protected-thunk exits
;;
;; dynamic-wind returns whatever is returned by
;; protected-thunk each time it exits
(dynamic-wind before-thunk protected-thunk after-thunk)
```

X-Ray Flourescence (XRF) spectrometry is used to determine the composition of
materials, for example in scrap yards and recyclying facilities. An industrial
XRF scanner can emit levels of radiation that would be quite dangerous. Imagine
a program to control to control such a device. It might have a function,
`scan-sample` which is the main entry-point, with the following requirements:

1. The sample chamber must have a door which prevents users from reaching inside
   when it is locked

2. The X-Ray emitter can only be turned on when the door is locked

3. The XRF spectrogram is recorded and made available to the user after a
   successful scan

One can imagine helper functions, called by `scan-sample`, for each of these operations:

```scheme
;; naive (unsafe!) implementation of scan-sample
(define (scan-sample)
    (lock-door)
    (energise-emitter)
    (record-data)
    (de-energize-emitter)
    (unlock-door))
```

As long as everything proceeds as expected, the naive definition implements the
requirements for scanning a sample laid out above. But sometimes unexpected
things can happen. Imagine that the XRF scanner is able to detect when a sample
must be repositioned within the chamber in order to obtain a complete
spectrogram. In that case, it needs to be able to interrupt the scanning
process, request that the user reposition the material within the chamber and
then resume from where it left off. Scheme continuations can be used to support
such requirement so long as sufficient care is taken to do so without violating
safety requirements.

What follows is a complete implementation of a simulation of `scan-sample` that
uses `stdout` to log the simulated operations. It uses continuations to throw
resumable exceptions when user intervention is required together with
`dynamic-wind` to ensure that the emitter is always off when the door is
unlocked.

<a id="drill-hole"></a>

```scheme
;; demonstrates:
;;
;; - continuations for non-sequential flow-of-control
;;
;; - dynamic-wind to protect blocks of code when using non-sequential flows-of-control
;;
;; - locally defined variables for encapsulation
;;
;; - macros for modularity

;; note from the output that the emitter is only on when the door is locked and the
;; sample is only repositioned when the door is unlocked. note also that the counter
;; shows that not only are the safety constraints being respected, the operations are
;; being done in the correct order

(define (scan-sample)

  (let ((display-message (lambda (message sample)
                           ;; common helper that prints a message to stdout
                           (display (string-join (list message (number->string sample) "\n") "")))))

    ;; deliberately not mutually-callable procedures representing various states of the xrf
    ;; spectrometer hardware
    (let ((lock-door (lambda ()
                       ;; simulate locking the sample chamber door
                       (display "door locked\n")))
          (energize-emitter (lambda ()
                              ;; simulate turning on the x-ray emitter
                              (display "emitter energized\n")))
          (de-energize-emitter (lambda ()
                                 ;; simulate turning off the x-ray emitter
                                 (display "emitter de-energized\n")))
          (unlock-door (lambda ()
                         ;; simulate unlocking the sample chamber door
                         (display "door unlocked\n")))
          (record-data (lambda (sample)
                         ;; simulate recording a xrf spectrogram
                         (call/cc (lambda (return)
                                    (display-message "scanning " sample)
                                    (display "please reposition sample\n")
                                    (set! sample (call/cc (lambda (resume) (return resume))))
                                    (display-message "scanning " sample)
                                    (display "please reposition sample\n")
                                    (set! sample (call/cc (lambda (resume) (return resume))))
                                    (display-message "scanning " sample)
                                    (display "data recorded\n"))))))

      (let-syntax ((with-emitter-energized (syntax-rules ()
                                             ;; ensure x-ray emitter is on only while the protected
                                             ;; forms are executing
                                             ((with-emitter-energized protected ...)
                                              (dynamic-wind
                                                energize-emitter
                                                (lambda () protected ...)
                                                de-energize-emitter))))
                   (with-door-locked (syntax-rules ()
                                       ;; ensure the sample chamber door is locked while the
                                       ;; protected forms are executing
                                       ((with-door-locked protected ...)
                                        (dynamic-wind
                                          lock-door
                                          (lambda () protected ...)
                                          unlock-door)))))

        (letrec ((count 1)
                 (resume (with-door-locked (with-emitter-energized (record-data count)))))
          ;; keep scanning and following prompts to reposition the sample until record-data
          ;; signals it is finished by not returning a continuation
          (if (procedure? resume)
              (begin
                (display-message "repositioning " count)
                (set! count (+ count 1))
                (resume count))
              (display-message "samples scanned: " count)))))))
```

See [xrf.scm](xrf.scm)


While all of the subroutines and helpers are defined inside the body of
`scan-sample` for encapsulation, the various `let`, `let-syntax` and `letrec`
forms are nested in ways that also helps enforce the requirements. In
particular, none of the main subroutines, `lock-door`, `energize-emitter` and so
on, can call one another because they are all deliberately defined in a single
`let` (_not_ `letrec`). Only the `display-message` helper is visible to the
entire body of `scan-sample` because it is in its own outermost `let`. The only
`letrec` is the innermost scope, and it is used instead of `let` only to allow
the invocation of `record-data` to receive `count` as a parameter without having
to introduce yet another level of lexical scope nesting.

Here is the result of invoking `(scan-sample)`:

```
> (scan-sample)
door locked
emitter energized
scanning 1
please reposition sample
emitter de-energized
door unlocked
repositioning 1
door locked
emitter energized
scanning 2
please reposition sample
emitter de-energized
door unlocked
repositioning 2
door locked
emitter energized
scanning 3
data recorded
emitter de-energized
door unlocked
samples scanned: 3
```

As can be seen from the output, the correct set of operations are performed, in
the correct order relative to one another so as to conform to the safety
requirements. The numeric counter shows that the operations are also carried out
in the correct order in regards to the end-to-end flow. This means that
`record-data` is actually completely sequential from its own point of view while
the body of `scan-sample` uses the [invocation of a continuation in tail
position](tail-recursion.md) to turn itself into a loop. The invocations of the
_before_ and _after_ logic by the various subroutines is encapsulated in the
`with-door-locked` and `with-emitter-energized` special forms.