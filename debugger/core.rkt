;; Debugger implementation
#lang at-exp racket/base

(require racket/match
	 racket/string
	 readline/readline
	 "command.rkt"
	 "breakpoints.rkt"
	 (prefix-in vm: "../vm/core.rkt")
	 "../vm/registers.rkt"
	 "../vm/opcodes.rkt"
	 "../vm/disassembler.rkt"
	 "../utils/readline.rkt")

(provide run-debugger)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readline history filename
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (history-filename)
  (define homedir (find-system-path 'home-dir))
  (path->string (build-path homedir ".vmdb_history")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define program-loaded  (make-parameter #f))
(define program-running (make-parameter #f))
(define mem   #f)
(define ip    #f)
(define bp    #f)
(define sp    #f)
;; Original register values
(define ip0 0)
(define bp0 0)
(define sp0 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debugger functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (do-load filename)
  (unless (file-exists? filename)
    (raise-user-error (format "load: could not find file ~a" filename)))
  (vm:load filename)
  (set! ip (get-vm-register 'ip))
  (set! bp (get-vm-register 'bp))
  (set! sp (get-vm-register 'sp))
  (set! ip0 (ip))
  (set! bp0 (bp))
  (set! sp0 (sp))
  (set! mem (vm-memory (current-vm)))
  (program-loaded #t))

(define (rerun?)
  (eprintf "Program is already running. Start again? ")
  (let ([l (read-line)])
    (and (string? l)
	 (or (string-ci=? "y" l)
	     (string-ci=? "yes" l)))))

(define (dbg-run)
  (when (or (not (program-running))
	    (and (program-running) (rerun?)))
    (program-running #t)
    (ip ip0)
    (bp bp0)
    (sp sp0)
    (continue)))

(define (continue)
  (letrec ([iter
	    (lambda ()
	      (define result (step mem ip))
	      (cond
	       [(breakpoint-at? (ip))
		(printf "Breakpoint reached at 0x~a~%" (number->string (ip) 16))]
	       [(= result END)
		(printf "Program terminated.~%")
		(program-running #f)]
	       [else (iter)]))])
    (iter)))

(define (disasm)
  (define ptr (make-parameter 0))
  (letrec ([loop
	    (lambda ()
	      (define line (disassemble ip0 mem ptr))
	      (when line
		(displayln line)
		(loop)))])
    (loop))
  (printf "~%"))


(define (do-print what [radix 10])
  (define name (case radix
		 [(10) "print"]
		 [(16) "print/x"]
		 [(2)  "print/b"]))
  (cond
   [(or (string=? what "registers")
	(string=? what "regs"))
    (print-registers radix)]
   [else
    (define bc (register->bytecode (string->symbol what)))
    (if bc
	(print-register bc radix)
	(raise-user-error (format "~a: unknown register ~a" name what)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define S string-append)

;; Register commands
(define (register-commands)
  (register-command "quit" "q"
		    0 0
		    #f #f
		    "- quits the debugger"
		    (lambda (args) 'quit))

  (register-command "help" "h"
		    0 0
		    #f #f
		    "- show this help page"
		    (lambda (args) (displayln commands-help)))

  (register-command "load" "l"
		    1 1
		    #f #f
		    "<file> - load bytecode from file"
		    (lambda (args) (do-load (car args))))

  (register-command "disasm" "d"
		    0 0
		    #t #f
		    "- disassemble loaded program"
		    (lambda (args) (disasm)))

  (register-command "run" "r"
		    0 0
		    #t #f
		    "- run program loaded through load"
		    (lambda (args) (dbg-run)))

  (register-command "cont" "c"
		    0 0
		    #t #t
		    "- continue program execution"
		    (lambda (args) (continue)))

  (register-command "step" "s"
		    0 0
		    #t #t
		    "- execute next instruction and stop"
		    (lambda (args) (step mem ip)))

  (register-command "break" "b"
		    1 1
		    #f #f
		    "<address> - break execution of program at address"
		    (lambda (args) (add-breakpoint (string->number (car args) 16))))

  (register-command "list-breakpoints" "lb"
		    0 0
		    #f #f
		    "- list current breakpoints"
		    (lambda (args) 
		      (printf "Breakpoints:~%")
		      (for-each (lambda (brk) 
				  (printf "@ 0x~a (~a)~%"
					  (number->string (car brk) 16)
					  (or (and (cdr brk) "enabled") "disabled")))
				(list-breakpoints))))

  (register-command "enable-breakpoint" "eb"
		    1 1
		    #f #f
		    "<address> - enable previously defined breakpoint"
		    (lambda (args) (enable-breakpoint (string->number (car args) 16))))
  
  (register-command "enable-breakpoints" "ebs"
		    0 0
		    #f #f
		    "- enable all previously defined breakpoints"
		    (lambda (args) (enable-breakpoints)))

  (register-command "disable-breakpoint" "db"
		    1 1
		    #f #f
		    "<address> - disable previously defined breakpoint"
		    (lambda (args) (disable-breakpoint (string->number (car args) 16))))
  
  (register-command "disable-breakpoints" "dbs"
		    0 0
		    #f #f
		    "- disable all previously defined breakpoints"
		    (lambda (args) (disable-breakpoints)))
  
  (register-command "remove-breakpoint" "rb"
		    1 1
		    #f #f
		    "<address> - remove previously defined breakpoint"
		    (lambda (args) (remove-breakpoint (string->number (car args) 16))))

  (register-command "print" "p"
		    1 1
		    #t #f
		    @S{registers - print all registers
				   regs - print all registers
				   <register> - print register value}
		    (lambda (args) (do-print (car args))))

  (register-command "print/x" "p/x"
		    1 1
		    #t #f
		    "- hexadecimal output version of print"
		    (lambda (args) (do-print (car args) 16)))

  (register-command "print/b" "p/b"
		    1 1
		    #t #f
		    "- binary output version of print"
		    (lambda (args) (do-print (car args) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (handle-line line)
  (match-define (list-rest cmd args)
		(string-split line " "))
  (invoke-command cmd args (program-loaded) (program-running)))

(define (read-one-line)
  (define line (readline "vmdb> "))
  (when (string? line)
    (with-handlers ([exn:fail:user?
		     (lambda (e) (eprintf "error: ~a~%" (exn-message e)))])
      (let ([res (handle-line (string-trim line " " #:repeat? #t))])
	(when res
	    (add-history line))
	res))))

(define (cli)
  (letrec ([loop (lambda () 
		   (unless (eq? 'quit (read-one-line))
		     (loop)))])
    (loop)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugger entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (run-debugger memsize)
  (register-commands)
  (load-history (history-filename))
  (vm:init (memsize))
  (cli)
  (save-history (history-filename))
  (printf "Bye.~%")
  (values))
