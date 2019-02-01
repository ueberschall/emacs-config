;; ~/.emacs.d/cpp_ide.el
;; Einstellungen für Emacs als C/C++ IDE werden hier vorgenommen
;;
;; *** Wichtige Tastenkombinationen ***
;; M-.: Zur Definition springen
;; M-,: Von Definition wieder zurückspringen
;; C-j: Alle im Projekt befindlichen Tags durchsuchen
;; C-c g r: Referenzen anzeigen
;; <f5>: Kompilieren (Prefix ändert den Kompilier-Befehl (C-u <f5>))
;;
;; *** Wichtige Funktionen ***
;; helm-gtags-show-stack: Zeige die Historie aller besuchten Tags an

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-general)
(require 'setup-helm)
(require 'setup-helm-gtags)
;; (require 'setup-ggtags)
;; (require 'setup-c)
(require 'setup-cedet)
;;(require 'setup-editing)

;;(c-set-style "ana_style")
