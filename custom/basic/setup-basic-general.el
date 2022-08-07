;; Maximieren der Emacs-Frames
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Die Mode Line wird angepasst
(setq-default mode-line-format
              '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified
                mode-line-remote mode-line-frame-identification mode-line-buffer-identification " (" mode-name ") "
                mode-line-position (vc-mode vc-mode) " "
                ))

(setq-default mode-line-buffer-identification
              (list (propertize
                     "%12b"
                     'face 'mode-line-buffer-id
                     'help-echo
                     '(format "%s\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                              (buffer-file-name))
                     'mouse-face 'mode-line-highlight
                     'local-map mode-line-buffer-identification-keymap)))

;; Zeilennummern aktivieren
(global-linum-mode t)

;; Die Schriftart auf Ubuntu-Mono einstellen
(set-face-attribute 'default nil :font "Ubuntu Mono-14")

;; Sicherung von Buffern
;; Setze das Verzeichnis für die Backups
(setq
   backup-by-copying t
   backup-directory-alist
   '(("." . "~/.backups"))
)

;; Setze das zeitliche Intervall zwischen automatischen Sicherungen
(setq auto-save-timeout 180)

;; Ausschalten der Werkzeugleiste
(tool-bar-mode -1)

;; Anschalten der Spalten-Nummer in der Mode Line
(column-number-mode 1)

;; Verhindere dass der Begrüßungsschirm angezeigt wird
(setq inhibit-startup-screen t
      initial-scratch-message ";; Scratch\n\n")

;; Mache "Ja-oder-Nein"-Abfragen einfacher
(defalias 'yes-or-no-p 'y-or-n-p)

;; Automatisches Schließen von Klammern
(electric-pair-mode 1)

;; Änderungen der Fenster-Konfiguration können rückgängig gemacht werden
(winner-mode 1)

;; Visual-Line-Mode global anschalten
(global-visual-line-mode 1)

;; Anzeigen von einander zugehörigen Klammern
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Benutze Leerzeichen für Einrückungen
(setq-default indent-tabs-mode nil)

;; Einstellen der Tab-Weite
(setq-default tab-width 4)

;; Keine Tabs für Einrückungen benutzen
(setq-default indent-tabs-mode nil)

;; Speichern des Desktops (Quelle: https://www.emacswiki.org/emacs/Desktop)
(setq desktop-load-locked-desktop t)

(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

(add-hook 'desktop-after-read-hook 'delete-desktop)

(add-hook 'kill-emacs-hook (lambda ()
                             (save-and-kill-outdated-buffers 30)
                             (session-save)))

(provide 'setup-basic-general)
