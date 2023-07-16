;; -*- geiser-scheme-implementation: guile -*-

;; simulated cnc drill referred to in
;; https://www.rader.us/software/scheme/dynamic-wind.html

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

;; wrap calls to complete drilling sequence in dynamic-wind
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

;; implement the complete simulated drilling scenario, including
;; exception handling and retries using continuations
(define (simulate-drilling)
  (let ((k (drill-hole)))
    (if (procedure? k)
	(begin
	  (wait-for-drill-to-cool)
	  (k 'resume))
	k)))
