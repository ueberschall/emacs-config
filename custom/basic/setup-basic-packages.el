;; *** Installieren der notwendigen Packages, falls dies noch nicht getan wurde ***
(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; *** Installiere fehlende Packete ***
;; dired+ ist hier nicht aufgeführt weil,
;; es nur manuell installiert werden kann und
;; nicht aus den Emacs-Packetquellen
(install-necessary-packages '(use-package
                               ace-window
                               exec-path-from-shell
                               magit
                               srefactor
                               sr-speedbar
                               company
                               projectile
                               helm
                               helm-projectile
                               org
                               ))

;; *** Lade benötigte Packete und Dateien ***
(require 'use-package)

(use-package ace-window)
(use-package exec-path-from-shell)
(use-package magit)
(use-package srefactor-lisp)
(use-package org)

(use-package dired+
  :config (diredp-toggle-find-file-reuse-dir 1))

(use-package sr-speedbar
  :init (setq speedbar-show-unknown-files t)
  :config (sr-speedbar-refresh-turn-off))

(use-package company
  :init
  (global-company-mode 1)
  (setq company-idle-delay 0)
  (setq-default company-backends
                '(company-capf company-bbdb company-cmake company-files
              (company-dabbrev-code company-keywords)
              company-oddmuse company-dabbrev)))

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

    (setq helm-echo-input-in-header-line t)
    
    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

    (setq helm-split-window-in-side-p t
;;          helm-autoresize-max-height 40
;;          helm-autoresize-min-height 20
          helm-mode-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-locate-fuzzy-match t)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (helm-autoresize-mode -1)

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
      (setq projectile-indexing-method 'alien)))

  :config
  (set-face-attribute 'helm-source-header nil :foreground "dark magenta" :weight 'bold
                      :background "black" :font "Ubuntu Mono-14")
  (set-face-attribute 'helm-selection nil :foreground "white" :background "SpringGreen4")
  (set-face-attribute 'helm-buffer-modified nil :foreground "RosyBrown"))

(provide 'setup-basic-packages)
