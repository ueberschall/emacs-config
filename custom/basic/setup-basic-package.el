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

(use-package projectile
  :config (projectile-mode 1))

(use-package helm
  :init
  (progn
    (require 'helm-config)
    (require 'helm-grep)
    ;; To fix error at compile:
    ;; Error (bytecomp): Forgot to expand macro with-helm-buffer in
    ;; (with-helm-buffer helm-echo-input-in-header-line)
    (if (version< "26.0.50" emacs-version)
        (eval-when-compile (require 'helm-lib)))

    (setq helm-split-window-in-side-p t
          setq helm-autoresize-max-height 40
          setq helm-autoresize-min-height 20
          helm-mode-fuzzy-match t
          helm-buffers-fuzzy-matching t)
    
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (helm-autoresize-mode 1)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

    (use-package helm-projectile
      :init
      (helm-projectile-on)
      (setq projectile-completion-system 'helm)
      (setq projectile-indexing-method 'alien))))

(provide 'setup-basic-package)
