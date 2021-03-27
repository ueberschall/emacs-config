;; Installieren der notwendigen Packages, falls dies noch nicht getan wurde
(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; Installiere fehlende Packete
;; (install-necessary-packages '(use-package
;;                                ace-window
;;                                exec-path-from-shell
;;                                magit
;;                                srefactor
;;                                sr-speedbar
;;                                company
;;                                projectile
;;                                helm
;;                                helm-projectile
;;                                org
;;                                flycheck
;;                                treemacs
;;                                academic-phrases
;;                                cyberpunk-theme
;;                                tron-legacy-theme
;;                                ))

;; dired+ und modeline-posn werden von EmacsWiki heruntergeladen
(let* ((diredplus-file (expand-file-name "dired+/dired+.el" "~/.emacs.d"))
      (diredplus-dir (file-name-directory diredplus-file)))
  (if (file-exists-p diredplus-file)
    (message "Dired+ does already exist")
  (unless (file-directory-p diredplus-dir)
    (make-directory diredplus-dir))
  (url-copy-file "https://www.emacswiki.org/emacs/download/dired%2b.el"
                 diredplus-file)))

(let* ((modeline-file (expand-file-name "modeline-posn/modeline-posn.el" "~/.emacs.d"))
      (modeline-dir (file-name-directory modeline-file)))
  (if (file-exists-p modeline-file)
    (message "Modeline-Posn does already exist")
  (unless (file-directory-p modeline-dir)
    (make-directory modeline-dir))
  (url-copy-file "https://www.emacswiki.org/emacs/download/modeline-posn.el"
                 modeline-file)))

;; Lade benötigte Packete und Dateien
(require 'use-package)

(use-package ace-window)
(use-package exec-path-from-shell)
(use-package magit)
(use-package srefactor-lisp)
(use-package org)

(use-package treemacs)

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
  :config
  (require 'helm-config)
  (require 'helm-grep)

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
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-locate-fuzzy-match t)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (helm-autoresize-mode -1)

  (set-face-attribute 'helm-source-header nil :foreground "dark magenta" :weight 'bold
                      :background "black" :font "Ubuntu Mono-14")
  (set-face-attribute 'helm-selection nil :foreground "white" :background "SpringGreen4")
  (set-face-attribute 'helm-buffer-modified nil :foreground "RosyBrown"))

(use-package helm-projectile
  :after helm
  :init
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien))

(use-package dired+
  :config (diredp-toggle-find-file-reuse-dir 1))

;; Das ist ein manuell kopiertes Package. "use-package" funktioniert
;; irgendwie nicht.
(require 'modeline-posn)

(provide 'setup-basic-packages)
