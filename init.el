;; ~/.emacs.d/init.el
;; Erstellt: 		14. April 2017
;; Aktualisiert: 	14. Januar 2021
;;
;; Die Init-Datei, die als Einsprungspunkt dient

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

;; Laden des "Cyberpunk"-Theme
(setq custom-safe-themes t)
(add-hook 'after-init-hook (lambda () (load-theme 'cyberpunk t)))
