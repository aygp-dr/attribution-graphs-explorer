#!/usr/bin/env guile
!#

(use-modules (ice-9 ftw)
             (ice-9 match)
             (ice-9 regex)
             (ice-9 rdelim)
             (srfi srfi-1))

;; Simple linter rules for Scheme code
(define *lint-rules*
  `(
    ;; Check for mismatched parentheses
    (parens . ,(lambda (line)
                 (let ((opens (count (lambda (c) (eq? c #\()) line))
                       (closes (count (lambda (c) (eq? c #\))) line)))
                   (unless (= opens closes)
                     (format #t "Mismatched parentheses: ~a opens, ~a closes~%" opens closes)))))
    
    ;; Check for trailing whitespace
    (whitespace . ,(lambda (line)
                     (when (string-match "[ \t]+$" line)
                       (format #t "Trailing whitespace~%"))))
    
    ;; Check for tabs (prefer spaces)
    (tabs . ,(lambda (line)
               (when (string-contains line "\t")
                 (format #t "Tab character found (prefer spaces)~%"))))
    
    ;; Check for lines over 80 characters
    (line-length . ,(lambda (line)
                      (when (> (string-length line) 80)
                        (format #t "Line too long: ~a characters (max 80)~%" (string-length line)))))
  ))

;; Process a single file
(define (lint-file filename)
  (format #t "Linting ~a~%" filename)
  (let ((errors 0))
    (call-with-input-file filename
      (lambda (port)
        (let loop ((line (read-line port)) (line-num 1))
          (unless (eof-object? line)
            (for-each
             (lambda (rule)
               (let ((check (cdr rule)))
                 (catch #t
                        (lambda () 
                          (format #t "~a:~a: " filename line-num)
                          (check line)
                          (set! errors (+ errors 1)))
                        (lambda (key . args) #f))))
             *lint-rules*)
            (loop (read-line port) (+ line-num 1))))))
    errors))

;; Lint all Scheme files in a directory
(define (lint-directory dir)
  (let ((total-errors 0))
    (ftw dir
         (lambda (filename statinfo flag)
           (if (and (eq? flag 'regular)
                    (string-suffix? ".scm" filename))
               (set! total-errors (+ total-errors (lint-file filename))))
           #t))
    total-errors))

;; Main entry point
(define (main args)
  (let ((dir (if (> (length args) 1) (cadr args) ".")))
    (let ((errors (lint-directory dir)))
      (if (> errors 0)
          (begin
            (format #t "Found ~a errors~%" errors)
            (exit 1))
          (begin
            (format #t "No errors found~%")
            (exit 0))))))

(main (command-line))