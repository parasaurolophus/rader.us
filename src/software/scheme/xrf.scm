; -*- mode: Racket; -*-

#!r6rs
(import (rnrs))

;; simulated xrf scanner referred to in
;; https://www.rader.us/software/scheme/dynamic-wind.html

;; demonstrates:
;;
;; - continuations for non-sequential flow-of-control
;;
;; - dynamic-wind to protect blocks of code when using
;;   non-sequential flows-of-control
;;
;; - locally defined variables for encapsulation
;;
;; - macros for modularity

;; expected output:
;;
;; > (scan-sample)
;; door locked
;; emitter energized
;; scanning 1
;; please reposition sample
;; emitter de-energized
;; door unlocked
;; repositioning 1
;; door locked
;; emitter energized
;; scanning 2
;; please reposition sample
;; emitter de-energized
;; door unlocked
;; repositioning 2
;; door locked
;; emitter energized
;; scanning 3
;; data recorded
;; emitter de-energized
;; door unlocked
;; samples scanned: 3

;; note from the output that the emitter is only on when the door is locked and the
;; sample is only repositioned when the door is unlocked. note also that the counter
;; shows that not only are the safety constraints being respected, the operations are
;; being done in the correct order

(define (scan-sample)

  (let ((display-message (lambda (message sample)
                           ;; common helper that prints a message to stdout
                           (display (string-append message (number->string sample) "\n")))))

    ;; deliberately not mutually-callable procedures representing various states of the xrf
    ;; spectrometer hardware
    (let ((lock-door (lambda ()
                       ;; simulate locking the sample chamber door
                       (display "door locked\n")))
          (energize-emitter (lambda ()
                              ;; simulate turning on the x-ray emitter
                              (display "emitter energized\n")))
          (de-energize-emitter (lambda ()
                                 ;; simulate turning off the x-ray emitter
                                 (display "emitter de-energized\n")))
          (unlock-door (lambda ()
                         ;; simulate unlocking the sample chamber door
                         (display "door unlocked\n")))
          (record-data (lambda (sample)
                         ;; simulate recording a xrf spectrogram
                         (call/cc (lambda (return)
                                    (display-message "scanning " sample)
                                    (display "please reposition sample\n")
                                    (set! sample (call/cc (lambda (resume) (return resume))))
                                    (display-message "scanning " sample)
                                    (display "please reposition sample\n")
                                    (set! sample (call/cc (lambda (resume) (return resume))))
                                    (display-message "scanning " sample)
                                    (display "data recorded\n"))))))

      (let-syntax ((with-emitter-energized (syntax-rules ()
                                             ;; ensure x-ray emitter is on only while the protected
                                             ;; forms are executing
                                             ((with-emitter-energized protected ...)
                                              (dynamic-wind
                                               energize-emitter
                                               (lambda () protected ...)
                                               de-energize-emitter))))
                   (with-door-locked (syntax-rules ()
                                       ;; ensure the sample chamber door is locked while the
                                       ;; protected forms are executing
                                       ((with-door-locked protected ...)
                                        (dynamic-wind
                                         lock-door
                                         (lambda () protected ...)
                                         unlock-door)))))

        (let* ((count 1)
               (resume (with-door-locked (with-emitter-energized (record-data count)))))
          ;; keep scanning and following prompts to reposition the sample until record-data
          ;; signals it is finished by not returning a continuation
          (if (procedure? resume)
              (begin
                (display-message "repositioning " count)
                (set! count (+ count 1))
                (resume count))
              (display-message "samples scanned: " count)))))))
