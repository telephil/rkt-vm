;; VM bytecode runner
#lang racket/base

(require racket/cmdline
         racket/path
         syntax/location
         setup/getinfo
         (prefix-in vm: "core.rkt"))

(define info (get-info/full
              (simplify-path (build-path (path-only (quote-module-path)) 'up))))

(define (show-version)
  (printf "VM v~a~%" (info 'version))
  (printf "Copyright (C) 2012 ~a~%" (info 'author)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command-line parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define memsize (make-parameter 1024))
(define dumpregs? (make-parameter #f))
(define filename (command-line
                  #:once-each
                  [("-V" "--version") "Display version information"
                   (show-version) (exit 0)]
                  [("-s" "--memsize") size "Memory size (in bytes)"
                   (memsize (string->number size))]
                  [("-d" "--dump-registers") () "Dump registers"
                   (dumpregs? #t)]
                  #:args (filename)
                  filename))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(vm:init (memsize))
(vm:load filename)
(vm:run)
(when (dumpregs?)
  (vm:print-registers 10))
(values)
