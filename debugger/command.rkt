;; Definition of a debugger command
#lang racket/base

(require racket/contract/base
         racket/bool)


;; (provide/contract
;;  [commands-help string?]
;;  [register-command
;;   (string? (or/c string? #f) integer? integer? boolean? string? procedure? . -> . void)]
;;  [invoke-command
;;   (string? list? boolean? . -> . any/c)])

(provide commands-help register-command invoke-command)

(struct command (name
                 short-name
                 min-args
                 max-args
                 need-loaded?
                 need-running?
                 doc
                 proc))


(define commands (make-hash))
(define commands-help "List of available commands:\n\n")

(define (register-command name short-name min-args max-args need-loaded? need-running? doc proc)
  (let ([cmd (command name short-name
                      min-args max-args
                      need-loaded? need-running?
                      doc
                      proc)])
    (hash-set! commands name cmd)
    (when short-name
      (hash-set! commands short-name cmd))
    (set! commands-help
          (string-append commands-help
                         "  " name
                         (if short-name (format " (~a)" short-name) "")
                         " "
                         doc
                         "\n")))
  (values))

(define (invoke-command name args program-loaded? program-running?)
  (define cmd (hash-ref commands name #f))
  (define argc (or (and (not (null? args)) (length args)) 0))
  (cond
   [(false? cmd)
     (raise-user-error (format "~a: unknown command" name))]
   [(< argc (command-min-args cmd))
     (raise-user-error (format "~a: not enough arguments (expected at least ~a)"
                               name
                               (command-min-args cmd)))]
   [(> argc (command-max-args cmd))
     (raise-user-error (format "~a: too much arguments (expected at most ~a)"
                               name
                               (command-max-args cmd)))]
   [(and (command-need-loaded? cmd) (not program-loaded?))
     (raise-user-error (format "~a: no program loaded" name))]
   [(and (command-need-running? cmd) (not program-running?))
     (raise-user-error (format "~a: no program running" name))]
   [else
    ((command-proc cmd) args)]))
