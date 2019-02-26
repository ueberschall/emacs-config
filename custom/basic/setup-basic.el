;; *** Maximieren des Emacs-Frames ***
;; The bottom commands maximize the window to full size
(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))

;; *** Die Mode Line wird angepasst
(setq-default mode-line-format
              '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified
               mode-line-remote mode-line-frame-identification mode-line-buffer-identification "  "
               mode-line-position (vc-mode vc-mode) "  "
               (:eval (substring (system-name) 0 (string-match "\\..+" (system-name))))
               ":" default-directory
               mode-line-end-spaces))

;; *** Zeilennummern aktivieren ***
(global-linum-mode t)

;; *** Die Schriftart auf Ubuntu-Mono einstellen ***
(set-face-attribute 'default nil :font "Ubuntu Mono-12")
(set-frame-font "Ubuntu Mono-12" nil t)

;; *** Sicherung von Buffern ***
;; Setze das Verzeichnis für die Backups
(setq
   backup-by-copying t
   backup-directory-alist
   '(("." . "~/.backups"))
)

;; Setze das zeitliche Intervall zwischen automatischen Sicherungen
(setq auto-save-timeout 180)

;; *** Ausschalten der Werkzeug-, Scrollbar- und Menüleiste ***
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; *** Verhindere dass der Begrüßungsschirm angezeigt wird ***
(setq inhibit-startup-screen t)

;; *** Mache "Ja-oder-Nein"-Abfragen einfacher ***
(defalias 'yes-or-no-p 'y-or-n-p)

;; *** Automatisches Schließen von Klammern ***
(electric-pair-mode 1)

;; *** Änderungen der Fenster-Konfiguration können rückgängig gemacht werden ***
(winner-mode 1)

;; *** Anzeigen von einander zugehörigen Klammern ***
(show-paren-mode 1)
(setq show-paren-delay 0)

;; *** Benutze Leerzeichen für Einrückungen ***
(setq-default indent-tabs-mode nil)

;; *** Einstellen der Tab-Weite ***
(setq-default tab-width 4)

;; *** Keine Tabs für Einrückungen benutzen ***
(setq-default indent-tabs-mode nil)

(require 'setup-desktop)
(require 'setup-basic-package)
(require 'setup-basic-keymap)

(provide 'setup-basic)
