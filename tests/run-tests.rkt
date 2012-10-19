#lang racket/base

(require rackunit
         racket/port
         racket/string
         racket/date)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test-file? : string? -> boolean?
;; check if a file is a test file
(define (test-file? filename)
  (regexp-match #rx"test-.*\\.rkt" filename))

;; test-file-list : -> list?
;; return a list of test files from current-directory
(define (test-file-list)
  (filter test-file? 
          (map path->string (directory-list))))

;; clean-datetime-string : void -> void
(define (clean-datetime-string)
  (foldl (lambda (from to result) 
           (string-replace result from to))
         (date->string (seconds->date (current-seconds)) #t)
         '(":" "-" " ") 
         '(""  ""  "_")))

;; report-file-name : -> string?
(define (errors-file-name)
  (date-display-format 'iso-8601)
  (format "test-errors-~a.txt" (clean-datetime-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; total number of tests executed
(define total   (make-parameter 0))
;; number of tests that succeeded
(define success (make-parameter 0))

;; redefine the custom check parameter proc
;; to count total and succeeded tests
(define (custom-check-around thunk)
  (total (add1 (total)))
  (with-handlers ([(lambda (e) #t) (current-check-handler)])
    (when (thunk)
      (success (add1 (success)))))
  (void))

;; print-test-report : void -> void
;; print test report
(define (print-test-report)
  (define failed (- (total) (success)))
  (printf "~a (total: ~a / failed: ~a)~%"
          (if (zero? failed) "SUCCESS" "FAILURE")
          (total) failed)
  (void))

;; run-tests : void -> void
;; run all test files
(define (run-tests)
  (for ([f (test-file-list)])
    (printf ">>> ~a: " f)
    (parameterize ([total 0]
                   [success 0])
      (dynamic-require (list 'file f) #f)
      (print-test-report)))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
(parameterize ([current-check-around custom-check-around])
  (call-with-output-file (errors-file-name) #:exists 'append
    (λ (out)
      (parameterize ([current-error-port out])
        (run-tests)))))
