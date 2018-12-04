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

(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-general)
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)
  (require 'setup-helm)
  (require 'setup-helm-gtags))
;; (require 'setup-ggtags)
(require 'setup-c)
(require 'setup-cedet)
(require 'setup-editing)



;; *** function-args wird hier nur eingebunden um Zugriff auf moo-jump-local zu haben ***
;; (require 'function-args)
;; (fa-config-default)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
