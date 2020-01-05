;; ~/.emacs.d/init.el
;; Erstellt: 		14. April 2017
;; Aktualisiert: 	31. Januar 2019
;;
;; Die Init-Datei, die als Einsprungspunkt dient
;;
;; *** Ergänze die Suchpfade ***
(add-to-list 'load-path "~/.emacs.d/dired+")
(add-to-list 'load-path "~/.emacs.d/custom/basic")
(add-to-list 'load-path "~/.emacs.d/custom/cpp_ide")
(add-to-list 'load-path "~/.emacs.d/custom/python_ide")
(add-to-list 'load-path "~/.emacs.d/custom/tex")
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom/themes")

;; *** Laden der Basis-Konfiguration ***
(require 'setup-basic)

;; *** Lade die Einstellungen für eine bestimmte Programmiersprache ***
(setq ide-mode (load-ide-mode))

;; *** Die Fensterleiste wird so eingestellt, dass der Name des aktuellen Buffers
;; *** angezeigt wird ***
(setq frame-title-format
      (list ide-mode "   -   Emacs " emacs-version))

;; *** Laden des "tron"-Themes, das aus den offiziellen Packetquellen stammt ***
(setq custom-safe-themes t)
(add-hook 'after-init-hook (lambda () (load-theme 'tron)))
