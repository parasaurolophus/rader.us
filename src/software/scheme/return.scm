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
        (lambda (result resume) handler ...))))))
