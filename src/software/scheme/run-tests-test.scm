#!r6rs
(import (rnrs) (run-tests))

(define (run-tests-test)
  (run-tests
   (lambda () (assert-equal 0 0 'assert-equal-1 "passing test of assert-equal"))
   (lambda () (assert-equal 0 1 'assert-equal-2 "failing test of assert-equal"))
   (lambda () (assert-true #t 'assert-true-3 "passing test of assert-true"))
   (lambda () (assert-true #f 'assert-true-4 "failing test of assert-true"))
   (lambda () (assert-false #t 'assert-false-5 "failing test of assert-false"))
   (lambda () (assert-false #f 'assert-false-6 "passing test of assert-false"))
   (lambda () (fail 'fail-7 "test of fail"))
   (lambda () (error 'run-tests "test that deliberately throws an error with irritants") 'foo 'bar)
   ;; no-assertion test that passes
   (lambda () (+ 1 2))))

(run-tests-test)
