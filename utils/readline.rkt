;; Readline extensions
#lang racket/base

(require racket/file
	 readline/readline)

(provide load-history
	 save-history)

;; Load readline history from filename
(define (load-history filename)
  (when (file-exists? filename)
    (for-each 
     (lambda (line) (add-history line))
     (file->lines filename))))

;; Save current readline history to filename
(define (save-history filename)
  (define lines (for/list ([i (in-range (history-length))])
		  (history-get i)))
  (display-lines-to-file lines filename #:exists 'replace))

