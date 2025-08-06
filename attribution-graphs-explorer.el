;;; attribution-graphs-explorer.el --- Emacs configuration for Attribution Graphs Explorer

;;; Commentary:
;; This file sets up Emacs for Scheme development with Guile3,
;; Geiser, Paredit, and Org-mode support for the Attribution Graphs Explorer project.

;;; Code:

;; Package initialization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install required packages if not present
(defvar attribution-graphs-packages
  '(geiser
    geiser-guile
    paredit
    org
    tramp))

(dolist (pkg attribution-graphs-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; Project configuration
(defvar attribution-graphs-root
  (or (getenv "PROJECT_ROOT")
      (file-name-directory load-file-name)))

(defvar attribution-graphs-name
  (or (getenv "PROJECT_NAME")
      "attribution-graphs-explorer"))

;; Geiser configuration for Guile
(require 'geiser)
(require 'geiser-guile)
(setq geiser-guile-binary "guile3")
(setq geiser-repl-history-filename
      (expand-file-name ".geiser-history" attribution-graphs-root))
(setq geiser-guile-load-path
      (list attribution-graphs-root
            (expand-file-name "attribution-graphs" attribution-graphs-root)))

;; Paredit mode for structured editing
(require 'paredit)
(add-hook 'scheme-mode-hook #'paredit-mode)
(add-hook 'geiser-repl-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

;; Org-mode configuration
(require 'org)
(setq org-babel-load-languages
      '((scheme . t)
        (emacs-lisp . t)
        (shell . t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-load-languages)
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; TRAMP configuration for remote editing
(require 'tramp)
(setq tramp-default-method "ssh")

;; File associations
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . scheme-mode))

;; Custom functions for project navigation
(defun attribution-graphs-open-org ()
  "Open the main org file for the project."
  (interactive)
  (find-file (expand-file-name "attribution-graphs-explorer.org" attribution-graphs-root)))

(defun attribution-graphs-run-examples ()
  "Run all examples in the project."
  (interactive)
  (let ((default-directory attribution-graphs-root))
    (compile "gmake run")))

(defun attribution-graphs-run-tests ()
  "Run project tests."
  (interactive)
  (let ((default-directory attribution-graphs-root))
    (compile "gmake test")))

;; Keybindings
(global-set-key (kbd "C-c a o") 'attribution-graphs-open-org)
(global-set-key (kbd "C-c a r") 'attribution-graphs-run-examples)
(global-set-key (kbd "C-c a t") 'attribution-graphs-run-tests)

;; Display startup message
(message "Attribution Graphs Explorer Emacs environment loaded.")
(message "Project root: %s" attribution-graphs-root)
(message "Keybindings: C-c a o (open org), C-c a r (run examples), C-c a t (run tests)")

(provide 'attribution-graphs-explorer)
;;; attribution-graphs-explorer.el ends here