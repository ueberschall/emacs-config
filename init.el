;; This file is the entry point for the custom configuration.
;; To load this file write the following into your .emacs file:

;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

(setq byte-compile-warnings '(cl-functions))

;; Add the 'custom' directory to the load path. 
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

;; Setup Emacs for all my workflows.
(require 'basic-setup)
(require 'cc-setup)
(require 'python-setup)
(require 'rust-setup)
(require 'tex-setup)

;; Load the functions for saving and restoring desktops.
(require 'desktop-saving)
(load-desktop desktop-base-file-name)
