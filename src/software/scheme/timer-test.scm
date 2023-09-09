#!r6rs
; import the compatibility library and timer/timer modules
(import (rnrs) (timer timer))

; start a timer that writes "expired" to stdout after 3 seconds
(define timer (start-timer
               3000.0
               (lambda ()
                 (display "expired\n"))))

; show that timer? returns true for an object returned by start-timer
(display (timer? timer))
(newline)

; write "in main thread" to stdout with the timer semaphore locked
(call-with-timer-semaphore
 timer
 (lambda ()
   (display "in main thread\n")))
