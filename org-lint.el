;; Simple org-mode linting script
(require 'org)
(require 'org-lint)

(defun org-lint-file (file)
  "Run org-lint on FILE and print results."
  (with-current-buffer (find-file-noselect file)
    (let* ((checks (org-lint-select-checkers))
           (errors (org-lint-collect checks)))
      (if errors
          (progn
            (message "Org-lint found %d errors in %s:" (length errors) file)
            (dolist (err errors)
              (let* ((line (car err))
                     (checker (nth 1 err))
                     (msg (nth 2 err)))
                (message "Line %d: [%s] %s" line checker msg)))
            (kill-emacs 1))
        (message "Org-lint: No errors found in %s" file)
        (kill-emacs 0)))))

;; Command line processing
(when (> (length command-line-args-left) 0)
  (org-lint-file (car command-line-args-left)))