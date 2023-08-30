#!r6rs
(library
 (run-tests)

 (export assert-true assert-false assert-equal assert-eqv assert-eq fail run-tests)

 (import (rnrs))

 (define (make-assertion-violation-condition who message irritants)
   (let ((simple-conditions (list (make-assertion-violation))))
     (when who
       (set! simple-conditions (cons (make-who-condition who) simple-conditions)))
     (when message
       (set! simple-conditions (cons (make-message-condition message) simple-conditions)))
     (unless (null? irritants)
       (set! simple-conditions (cons (make-irritants-condition irritants) simple-conditions)))
     (apply condition simple-conditions)))

 (define (display-condition condition)
   (if (assertion-violation? condition)
       (begin
         (display "assertion-violation")
         (when (who-condition? condition)
           (display " ")
           (display (condition-who condition)))
         (when (message-condition? condition)
           (display " ")
           (display (condition-message condition)))
         (when (irritants-condition? condition)
           (display " ")
           (display (condition-irritants condition))))
       (display condition)))

 (define (assert-true test who message . irritants)
   (unless test
     (raise (make-assertion-violation-condition who message irritants))))

 (define (assert-false test who message . irritants)
   (when test
     (raise (make-assertion-violation-condition who message irritants))))

 (define (assert-equal obj1 obj2 who message . irritants)
   (unless (equal? obj1 obj2)
     (raise (make-assertion-violation-condition who message (cons obj1 (cons obj2 irritants))))))

 (define (assert-eqv obj1 obj2 who message . irritants)
   (unless (eqv? obj1 obj2)
     (raise (make-assertion-violation-condition who message (cons obj1 (cons obj2 irritants))))))

 (define (assert-eq obj1 obj2 who message . irritants)
   (unless (eq? obj1 obj2)
     (raise (make-assertion-violation-condition who message (cons obj1 (cons obj2 irritants))))))

 (define (fail who message . irritants)
   (raise (make-assertion-violation-condition who message irritants)))

 (define (run-tests . tests)
   (let ((passed 0)
         (failed 0)
         (ran 0))
     (let loop ((tests tests))
       (unless (null? tests)
         (set! ran (+ ran 1))
         (guard (e (else (display-condition e)
                         (newline)
                         (set! failed (+ failed 1))))
                ((car tests))
                (set! passed (+ passed 1)))
         (loop (cdr tests))))
     (let ((total (length tests)))
       (display ran)
       (display " out of ")
       (display total)
       (display " tests ran\n")
       (display passed)
       (display " passed\n")
       (display failed)
       (display " failed\n")))))
