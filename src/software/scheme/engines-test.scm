#!r6rs
(import (rnrs) (engines))

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
(define (engines-test count)
  (letrec ((infinite-loop-count 0)
           (finite-loop-count 0)
           (infinite-loop
            (lambda ()
              (decrement-timer!)
              (set! infinite-loop-count
                    (+ infinite-loop-count 1))
              (display "infinite loop")
              (newline)
              (infinite-loop)))
           (finite-loop
            (lambda (count)
              (decrement-timer!)
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

(engines-test 100)
