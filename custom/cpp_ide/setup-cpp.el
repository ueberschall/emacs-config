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

(require 'setup-cpp-packages)
(require 'setup-cpp-general)
(require 'setup-cpp-keymap)

(semantic-mode 1)

(provide 'setup-cpp)
