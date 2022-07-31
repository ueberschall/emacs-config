;; This file is the entry point for the custom configuration.
;; To load this file write the following into your .emacs/init.el file:

;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

;; Ergänze die Suchpfade 
(add-to-list 'load-path "~/.emacs.d/dired+")
(add-to-list 'load-path "~/.emacs.d/modeline-posn")
(add-to-list 'load-path "~/.emacs.d/custom/basic")
(add-to-list 'load-path "~/.emacs.d/custom/cpp")
(add-to-list 'load-path "~/.emacs.d/custom/python")
(add-to-list 'load-path "~/.emacs.d/custom/rust")
(add-to-list 'load-path "~/.emacs.d/custom/tex")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags")
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom/themes")

(setq byte-compile-warnings '(cl-functions))

;; Laden der Basis-Konfiguration
(require 'setup-basic)

;; Für jede Programmiersprache gibt es einen bestimmten IDE-Modus
(setq ide-mode (load-ide-mode))

;; Die Fensterleiste wird so eingestellt, dass der aktuelle IDE-Modus
;; zusammen mit der aktuellen Emacs-Version angezeigt wird.
(setq frame-title-format
      (list ide-mode "   -   Emacs " emacs-version))

;; Rufe in bestimmten Fällen Treemacs auf
(when (equal ide-mode "C/C++")
  (treemacs)
  (treemacs-toggle-fixed-width))

;; Laden des "Cyberpunk"-Theme
(setq custom-safe-themes t)
(add-hook 'after-init-hook (lambda () (load-theme 'cyberpunk t)))
