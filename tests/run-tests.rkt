#lang racket/base

(require rackunit
         racket/port
         racket/string
         racket/path
	 racket/cmdline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pad-number: number? integer? -> string?
(define (pad-number n len)
  (define s (number->string n))
  (string-append (make-string (- len (string-length s)) #\space) s))

;; clean-filename : string? -> string?
(define (clean-filename filename)
  (define path (file-name-from-path (string->path filename)))
  (define s (string-replace (path->string path) ".rkt" ""))
  (string-append s
   (make-string (- 16 (string-length s)) #\space)))

;; ansi-format : string? -> string?
(define (ansi-format message color)
  (define ESC #\u001b)
  (format "~a[~am~a~a[0m" ESC color message ESC))

(define RED 31)
(define GREEN 32)

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

;; format-test-report : string? -> string?
(define (format-test-report filename)
  (define failed (- (total) (success)))
  (format "~a - ~a - Run: ~a - Failed: ~a"
	  (if (zero? failed)
	       (ansi-format "✔" GREEN)
	       (ansi-format "✘" RED))
	  (clean-filename filename)
	  (pad-number (total) 3) failed))

;; run-tests : void -> (listof string?)
;; run all test files and return a list of test reports
(define (run-tests files)
  (for/list ([f files])
    (parameterize ([total 0]
                   [success 0])
      (dynamic-require (list 'file f) #f)
      (format-test-report f))))

;; display-summary : (listof string?) -> void
(define (display-summary reports)
  (displayln "")
  (displayln "Summary:")
  (displayln (make-string 48 #\-))
  (for-each displayln reports)
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main arg . args)
  (define files
    (command-line #:args files files))
  (parameterize ([current-check-around custom-check-around])
    (display-summary (run-tests files))))

(provide main)
