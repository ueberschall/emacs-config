;; This file is the entry point for the custom configuration.
;; To load this file write the following into your .emacs file:

;; (setq custom-file (expand-file-name "init.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

(setq byte-compile-warnings '(cl-functions))

;; Add the 'custom' directory to the load path. 
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags")

;; Setup Emacs for all my workflows.
(require 'basic-setup)

;; Right now the below setups are not necessary
;;
;; (require 'python-setup)
;; (require 'cc-setup)
;; (require 'rust-setup)
;; (require 'tex-setup)

;; Load the functions for saving and restoring desktops.
(require 'desktop-saving)
(load-desktop desktop-base-file-name)

(setq custom-file null-device)
