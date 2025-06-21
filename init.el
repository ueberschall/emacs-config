;; This file is the entry point for the custom configuration.
;; To load this file write the following into your .emacs file:

;; (setq custom-file (expand-file-name "init.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

(setq byte-compile-warnings '(cl-functions))
(setq native-comp-eln-load-path '("/home/zufall/.emacs-eln-cache/"))

;; Add the 'custom' directory to the load path. 
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

(require 'basic)
(require 'org-config)
(require 'desktop-config)

(session-restore)

(setq custom-file null-device)
