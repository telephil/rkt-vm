;; Implementation of the debugger CLI parser
#lang racket

(require "info.rkt"
	 "private/vm.rkt"
	 readline/readline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readline history helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (history-filename)
  (define homedir (find-system-path 'home-dir))
  (path->string (build-path homedir ".vmdb-history")))

(define (load-history)
  (define filename (history-filename))
  (when (file-exists? filename)
    (map (lambda (line) (add-history line))
	 (file->lines filename)))
  (values))

(define (save-history)
  (define filename (history-filename))
  (define lines (for/list ([i (in-range (history-length))])
		  (history-get i)))
  (display-lines-to-file lines filename #:exists 'replace))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define program-loaded (make-parameter #f))

(define (cmd-load args)
  (unless (= 1 (length args))
    (raise-user-error (format "load: invalid arguments ~a~%" args)))
  (match-define (list filename) args)
  (unless (file-exists? filename)
    (raise-user-error (format "load: could not find file ~a~%" filename)))
  (load-file filename)
  (program-loaded #t))

(define (cmd-run args)
  (unless (program-loaded)
    (raise-user-error "No program loaded"))
  (unless (null? args)
    (raise-user-error (format "run: invalid arguments ~a~%" args)))
  (run))

(define (cmd-step args)
  (unless (program-loaded)
    (raise-user-error "No program loaded"))
  (unless (null? args)
    (raise-user-error (format "step: invalid arguments ~a~%" args)))
  (define mem (vm-memory (current-vm)))
  (define ip (get-vm-register 'ip))
  (step mem ip))

(define (cmd-print args)
  (unless (= 1 (length args))
    (raise-user-error (format "print: invalid arguments ~a~%" args)))
  (define target (first args))
  (if (string=? (first args) "registers")
      (dump-registers)
      #f))

(define (cmd-help args)
  (displayln "Available commands:")
  (displayln "-------------------")
  (displayln "  load <filename>: Load given bytecode file in memory")
  (displayln "  run: Run loaded program")
  (displayln "  step: Run next instruction")
  (displayln "  print <parameter>: ")
  (displayln "    Where parameter is one of the following:")
  (displayln "      <register> : print the value of given register")
  (displayln "      registers  : print all registers")
  (displayln ""))

(define (cmd-quit args)
  (save-history)
  (printf "Bye.~%")
  (exit 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cli
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define command-handlers
  (make-hash
   (list
    (cons "quit"  cmd-quit)
    (cons "help"  cmd-help)
    (cons "load"  cmd-load)
    (cons "run"   cmd-run)
    (cons "step"  cmd-step)
    (cons "print" cmd-print))))

(define (handle-line line)
  (match-define (list-rest cmd args) (string-split line " "))
  (define handler (hash-ref command-handlers cmd #f))
  (if handler
      (handler args)
      #f))

(define (read-one-line)
  (define line (readline "vmdb> "))
  (when (string? line)
    (with-handlers ([exn:fail:user? (lambda (e) (printf "~a" (exn-message e)))])
      (if (handle-line (string-trim line " " #:repeat? #t))
	  (add-history line)
	  (printf "Invalid command~%")))))

(define (cli)
  (letrec ([loop (lambda () 
		   (read-one-line)
		   (loop))])
    (loop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frontend implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (show-version)
  (printf "VM Debugger v~a~%" vm-version)
  (printf "Copyright (C) 2012 ~a~%" vm-author))

;; Command-line parsing
(define memsize (make-parameter 1024))

;; Main
(define (main)
  (command-line
   #:once-each
   [("-V" "--version") "Display version information"
    (show-version) (exit 0)]
   [("-s" "--memsize") size "Memory size (in bytes)"
    (memsize (string->number size))])
  (show-version)
  (displayln "")
  (load-history)
  (create-vm (memsize))
  (cli)
  (values))

;; Execute main
(main)
