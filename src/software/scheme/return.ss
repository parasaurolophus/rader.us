#!r6rs

(library

 (return)

 (export call-with-return return with-return)

 (import (rnrs))

 (define current-continuation #f)

 (define (call-with-return thunk)
   (let ((saved current-continuation))
     (call/cc
      (lambda (k)
        (dynamic-wind
         (lambda () (set! current-continuation k))
         (lambda () (values (thunk) #f))
         (lambda () (set! current-continuation saved)))))))

 (define (return result)
   (call/cc
    (lambda (resume)
      (current-continuation result resume))))

 (define-syntax with-return
   (syntax-rules ()
     ((_ resumable (result resume) handler ...)
      (call-with-values
       (lambda ()
         (call-with-return
          (lambda () resumable)))
       (lambda (result resume) handler ...)))))

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
   (assert (= 42 (resume-test))))
