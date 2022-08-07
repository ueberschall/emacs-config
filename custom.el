;; This file is the entry point for the custom configuration.
;; To load this file write the following into your .emacs file:

;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

(setq byte-compile-warnings '(cl-functions))

;; Add the 'custom' directory to the load path. 
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))
;; (add-to-list 'load-path "~/.emacs.d/modeline-posn")
;; (add-to-list 'load-path "~/.emacs.d/custom/basic")
;; (add-to-list 'load-path "~/.emacs.d/custom/cpp")
;; (add-to-list 'load-path "~/.emacs.d/custom/python")
;; (add-to-list 'load-path "~/.emacs.d/custom/rust")
;; (add-to-list 'load-path "~/.emacs.d/custom/tex")
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/custom/themes")

;; Laden der Basis-Konfiguration
(require 'basic-setup)
(require 'cc-setup)
(require 'python-setup)
(require 'rust-setup)
(require 'tex-setup)
