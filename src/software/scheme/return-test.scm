#!r6rs
(import (rnrs) (return))

;; unit tests for return.scm library

(define (return-test)
  (let ((returnable
         (lambda ()
           (return 40)
           (error 'return-test "execution should not reach here"))))
    (with-return
      (returnable)
      (result _)
      result)))

(define (resume-test)
  (let ((resumable
         (lambda ()
           (call-with-values
            (lambda () (return 40))
            (lambda (value _) (+ value 1))))))
    (with-return
      (resumable)
      (result resume)
      (if (procedure? resume)
          (resume (+ result 1) #f)
          result))))

(assert (= 40 (return-test)))
(assert (= 42 (resume-test)))
