;; -*- geiser-scheme-implementation: guile -*-

;; Copyright 2016-2023 Kirk Rader
;;
;; Licensed under the Apache License, Version 2.0 (the "License"); you
;; may not use this file except in compliance with the License.  You
;; may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;; implied.  See the License for the specific language governing
;; permissions and limitations under the License.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Engines from Continuations
;;
;; Adapted from Dybvig and Hieb, "Engines from Continuations" [1988]
;;
;; "Engines" are an abstraction for decomposing a program into
;; procedures that can be scheduled to run for only a limited amount
;; of time. If an engine runs out of "fuel" before it has completed,
;; it can later be resumed from the point at which it was
;; interrupted. Using this simple mechanism it is trivial to implement
;; co-routines, a light-weight round-robin thread scheduler etc.
;;
;; In particular, an engine is a procedure of three arguments, created
;; from a procedure of zero argments. E.g.
;;
;;    (let ((my-engine (make-engine (lambda ()
;;                                     (decrement-timer)
;;                                     (display "engine running")
;;                                     (newline)))))
;;       (my-engine
;;          ;; the amount of "fuel" for this engine,
;;          ;; i.e. the number of times decruement-time
;;          ;; can be called before this engine expires
;;          1
;;          ;; the "completion" routine
;;          (lambda (value remaining-ticks)
;;             (display "engine returned ")
;;             (display value)
;;             (display " with ")
;;             (display remaining-ticks)
;;             (display " ticks remaining")
;;             (newline))
;;          ;; the "expiration" routine
;;          (lambda (new-engine)
;;             (display "engine interrupted, use the ")
;;             (display "given new engine to resume")
;;             (newline))))
;;
;; When invoked, the engine will call the procedure from which it was
;; created. In addition, the engine will interrupt the procedure after
;; the number of "ticks" specified by its first argument have elapsed.
;; If the engine's procedure returns before the given number of ticks
;; have elapsed, it invokes the procedure passed as its second
;; argument. If the engine's procedure is interrupted before
;; returning, the engine invokes the procedure passed as its third
;; argument. The former is passed the value returned by the engine's
;; procedure and the number of remaining ticks. The latter is passed a
;; new engine that is the continuation of the one that was
;; interrupted.
;;
;; NOTE WELL!!
;; ===========
;;
;; This implementation of engines does not rely on true threads as are
;; available in many modern Scheme implementations (e.g. Guile). Nor
;; does it assume any mechanism for overriding the built-in lambda,
;; let, letrec and similar special forms with versions that implicitly
;; invoke decrement-timer (q.v.), as was assumed in Dybvig and Hieb's
;; original papaer. This means that you *must* call decrement-timer
;; explicitly at strategic points in your engine procedures or else
;; they will never yield to other engines.
;;
;; Many modern Scheme implementations offer "apply hooks" and similar
;; mechanisms for injecting custom function calls down in the guts of
;; Scheme's run time machinery. You can use such mechanisms to arrange
;; to have decrement-timer called implicitly, but you may get
;; surprising results depending on the particular mechanism you
;; use. For example, a simple "apply hook" is likely not to be invoked
;; at exactly the right execution points for code that uses primarily
;; built-in special forms for which the compiler does not actually
;; emit function calls. Conversely, for code with many calls to actual
;; functions, decrementing the timer at each and every application not
;; only adds significant overhead but also means that you might need
;; to carefully tune the number of ticks you pass to each engine to
;; give each one a reasonably fair chance to run.
;;
;; For these reasons, it is actually far more reliable simply to
;; follow the policy assumed by this implementation and put the onus
;; on the programmer to call decrement-timer explicitly at sensible
;; "synchronization points" in engines. This does mean that an engine
;; is really just an elaborate version of "apply" if the programmer
;; fails to call decrement-timer often enough in inner loops.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: (start-timer ticks handler)
;;
;; Arrange to call handler after a specified number of calls to
;; decrement-timer (q.v.)
;;
;; ticks - The number of times decrement-timer can be called before
;; handler is invoked.
;;
;; handler - The procedure to call when the timer counts down to 0.
;;
;; Usage: (start-timer 2
;;                     (lambda ()
;;                        (display "timer expired")
;;                        (newline)))
;;        (decrement-timer)
;;        (decrement-timer)
;;
;; See: stop-timer, decrement-timer
(define start-timer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function (stop-timer)
;;
;; Stop the timer if it is currently running. This will prevent the
;; timer from invoking any previously specified handler.
;;
;; Usage: (stop-timer)
;;
;; See: start-timer, decrement-timer
(define stop-timer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decrement the timer.
;;
;; Invokes the current handler if the timer expires as a result of
;; this call.
;;
;; Usage: (decrement-timer)
;;
;; See: start-timer, stop-timer
(define decrement-timer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation of start-timer, stop-timer and decrement-timer
(let ((clock 0)
      (handler '()))

  (set! start-timer
        (lambda (ticks new-handler)
          (set! handler new-handler)
          (set! clock ticks)))

  (set! stop-timer
        (lambda ()
          (let ((remaining clock))
            (set! clock 0)
            (set! handler '())
            remaining)))

  (set! decrement-timer
        (lambda ()
          (when (> clock 0)
            (set! clock (- clock 1))
            (when (< clock 1)
              (let ((h handler))
                (stop-timer)
                (h)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: (make-engine thunk)
;;
;; Creates an engine from the given procedure.
;;
;; thunk - a zero-argument procedure
;;
;; Usage: (let ((engine
;;                 (make-engine
;;                    (lambda ()
;;                       (decrement-timer)
;;                       (display "engine running")
;;                       (newline)))))
;;           (engine
;;              1
;;              (lambda (result remaining-ticks)
;;                 (display "engine completed with value ")
;;                 (display result))
;;                 (display " and ")
;;                 (display remaining-ticks)
;;                 (display " remaining ticks")
;;                 (newline))
;;              (lambda (new-engine)
;;                 (display "engine expired; use the")
;;                 (display "given new-engine to resume")
;;                 (newline))))
;;
;; See: engine-return
(define make-engine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: (engine-return value)
;;
;; Return immediately from the currently executing engine.
;;
;; This will cause the currently active engine to immediately exit and
;; so for its completion handler to be invoked.
;;
;; value - The value to return as the result of the engine's computation.
;;
;; Usage: (engine-return 'my-value)
;;
;; See: make-engine
(define engine-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Classic" implementation of engines.
;;
;; This is adapted from Dybvig and Hieg, "Engines from Continuations"
;; [1988] with one modification.
;;
;; The original authors stuck exactly to the "traditional" definition
;; of engine behavior, which declared a normal return from an engine
;; procedure to be an error. (Hence the need for and frequent use of
;; the "make-simple-engine" helper function in the original text.)
;;
;; This version eliminates the need for "make-simple-engine" by
;; wrapping the invocation of the engine procedure in a call to
;; engine-return inside the body of make-engine.
(let ((active? #f)
      (do-return #f)
      (do-expire #f))

  (letrec
      ((timer-handler
        (lambda ()
          (start-timer (call/cc do-expire) timer-handler)))

       (new-engine
        (lambda (resume)
          (lambda (ticks return expire)
            (if active?
                (error 'engine "attempt to nest engines")
                (set! active? #t))
            ((call/cc
              (lambda (escape)
                (set! do-return
                      (lambda (value ticks)
                        (set! active? #f)
                        (escape (lambda () (return value ticks)))))
                (set! do-expire
                      (lambda (resume)
                        (set! active? #f)
                        (escape (lambda () (expire (new-engine resume))))))
                (resume ticks))))))))

    (set! make-engine
          (lambda (thunk)
            (new-engine
             (lambda (ticks)
               (start-timer ticks timer-handler)
               (engine-return (thunk))))))

    (set! engine-return
          (lambda (value)
            (if active?
                (let ((ticks (stop-timer)))
                  (do-return value ticks))
                (error 'engine "no engine running"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: (make-queue)
;;
;; Creates a simple FIFO queue.
;;
;; Usage: (let ((queue (make-queue)))
;;           (queue 'enqueue 1)
;;           (queue 'enqueue 2)
;;           (queue 'enqueue 3)
;;           (while (not (queue 'empty?))
;;              (display (queue 'dequeue))
;;              (newline)))
;;
;; See: empty-queue?, enqueue, dequeue
(define (make-queue)

  (let ((front '())
        (back '()))

    (lambda (message . arguments)

      (case message

        ((enqueue)
         (when (null? arguments)
           (error 'queue "missing argument to push"))
         (set! back (cons (car arguments) back)))

        ((dequeue)
         (when (null? front)
           (set! front (reverse back))
           (set! back '()))
         (when (null? front)
           (error 'queue "empty queue"))
         (let ((value (car front)))
           (set! front (cdr front))
           value))

        ((empty?)
         (and (null? front) (null? back)))

        (else
         (error 'queue "unsupported message" message))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro: (empty-queue? queue)
;;
;; "Canonical" syntax for (queue 'empty?)
(define-syntax empty-queue?
  (syntax-rules ()
    ((empty-queue? queue)
     (queue 'empty?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro: (enqueue value queue)
;;
;; "Canonical" syntax for (queue 'enqueue value)
(define-syntax enqueue
  (syntax-rules ()
    ((enqueue value queue)
     (queue 'enqueue value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro: (dequeue queue)
;;
;; "Canonical" syntax for (queue 'dequeue)
(define-syntax dequeue
  (syntax-rules ()
    ((dequeue queue)
     (queue 'dequeue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: (first-true . thunks)
;;
;; Return the value of the first of the given procedures to return a
;; value other than #f or #f if all of the procedures terminate with
;; the value #f.
;;
;; Uses engines to interleave execution of the given procedures such
;; that first-true will return if at least one of the procedures
;; returns a value other then #f even if one or more of the procedures
;; never returns.
;;
;; As with make-engine and engine-return, this is adapted from Dybvig
;; and Hieg, "Engines from Continuations" [1988]. It serves as a unit
;; test for engines. Note that this demonstrates the power of engines
;; to implement an extremely light-weight "threading" mechanism in
;; pure Scheme.
;;
;; See: make-engine, parallel-or
(define (first-true . thunks)
  (letrec ((engines
            ;; FIFO queue of engines to run
            (make-queue))
           (run
            ;; execute each engine in the queue, removing engines that
            ;; terminate, re-enqueueing ones that expire, until one
            ;; returns a value other than #f
            (lambda ()
              (if (empty-queue? engines)
                  #f
                  (let ((engine (dequeue engines)))
                    (engine
                     1
                     (lambda (result ticks) (or result (run)))
                     (lambda (engine) (enqueue engine engines) (run))))))))
    (for-each (lambda (thunk)
                (enqueue (make-engine thunk) engines))
              thunks)
    (run)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro: (parallel-or ...)
;;
;; Like (or ...) except that it will return if even one expression
;; returns a value other than #f, even if one or more expressions
;; never return.
;;
;; See: first-true
(define-syntax parallel-or
  (syntax-rules ()
    ((parallel-or e ...)
     (first-true (lambda () e) ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit test for engines.
;;
;; Invoke parallel-or on two expressions, the first of which never
;; returns.
;;
;; count - the argument to pass to finite-loop (q.v.)
(define (por count)
  (letrec ((infinite-loop-count 0)
           (finite-loop-count 0)
           (infinite-loop
            (lambda ()
              (decrement-timer)
              (set! infinite-loop-count
                    (+ infinite-loop-count 1))
              (display "infinite loop")
              (newline)
              (infinite-loop)))
           (finite-loop
            (lambda (count)
              (decrement-timer)
              (set! finite-loop-count
                    (+ finite-loop-count 1))
              (if (> count 0)
                  (begin
                    (display "finite loop count ")
                    (display count)
                    (newline)
                    (finite-loop (- count 1)))
                  (begin
                    (display "infinite-loop-count ")
                    (display infinite-loop-count)
                    (newline)
                    (display "finite-loop-count ")
                    (display finite-loop-count)
                    (newline)
                    #t)))))
    (parallel-or
     (infinite-loop)
     (finite-loop count))))
