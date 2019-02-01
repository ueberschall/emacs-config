;; *** Installieren der notwendigen Packages, falls dies noch nicht getan wurde ***
(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; *** Lade benötigte Packete und Dateien ***
(require 'use-package)

;;(use-package better-defaults)

(use-package exec-path-from-shell)

(use-package magit)

(use-package dired+)

(use-package ace-window)

(use-package sr-speedbar
  :init (setq speedbar-show-unknown-files t)
  :config (sr-speedbar-refresh-turn-off))

(provide 'setup-basic-package)
