#lang racket/base

(require racket/sequence
         racket/match
         racket/string
         readline/readline
         "../utils/readline.rkt")

(provide cli)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (handle-line handler line)
  (match-define (list-rest cmd args)
                (string-split line " "))
  (handler cmd args))

(define (read-one-line handler prompt)
  (define line (readline prompt))
  (when (string? line)
    (with-handlers ([exn:fail:user?
                     (lambda (e) (eprintf "error: ~a~%" (exn-message e)))])
      (let ([res (handle-line handler (string-trim line " " #:repeat? #t))])
        (when res
          (add-history line))
        res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cli #:handler [handler values]
             #:stop    [stop    #f]
             #:prompt  [prompt  "> "]
             #:history [history ".history"])
  (load-history history)
  (sequence-for-each values (in-producer read-one-line stop handler prompt))
  (save-history history))
