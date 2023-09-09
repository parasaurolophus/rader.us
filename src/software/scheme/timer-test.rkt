#lang racket
; import the timer/timer module
(require timer/timer)

; start a timer that writes "timer expired" to stdout after 3 seconds
(define timer (start-timer
               3000.0
               (lambda (message)
                 (display message)
                 (newline))
               #f
               "expired"))

; show that timer? returns true for an object returned by start-timer
(display (timer? timer))
(newline)

; write "in main thread" to stdout with the timer semaphore locked
(call-with-timer-semaphore
 timer
 (lambda (message)
   (display message)
   (newline))
 #f
 "in main thread")
