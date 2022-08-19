(require 'cc-functions)

;; Load the functions for the doxygen generation.
(require 'doxygen-generation)

;;--------------------------------------Configure packages -------------------

;; Configure C/C++-Mode
(use-package cc-mode
  :config
  (setq gc-cons-threshold 100000000
        gdb-many-windows t
        gdb-show-main t
        gud-tooltip-mode 1
        company-global-modes '(not gud-mode))
  (setq-local company-backends '())

  (c-add-style "ana_style"
               '("bsd"
                 (c-basic-offset . 4)
                 (c-offset-alists
                  (case-label . *))))

  (setq c-default-style "ana_style")
  (add-hook 'c-mode-common-hook (lambda ()
                                  (c-set-style "ana_style")))
  (add-hook 'c++-mode-hook (lambda ()
                             (c-set-offset 'innamespace 0)))

  :bind (:map c-mode-base-map
              ("RET" . cpp-newline)
              ("C-c =" . insert-double-line-comment-seperator)
              ("C-c -" . insert-single-line-comment-seperator)
              ("C-c d c" . extract-and-insert-doxygen-documentation-for-symbol-under-point)))

(use-package clang-format
  :config
  (add-hook 'before-save-hook (lambda ()
                                (when (and
                                       (derived-mode-p 'cc-mode)
                                       (file-exists-p (expand-file-name ".clang-format" (projectile-project-root))))
                                  (clang-format-buffer))))
  :bind (:map c-mode-base-map
              ("C-c f r" . clang-format-region)
              ("C-c f b" . clang-format-buffer)))

(use-package rtags
  :if (executable-find "rdm")
  :load-path "/usr/local/share/emacs/site-lisp/rtags"
  :config
  (setq rtags-path "/usr/local/bin"
        rtags-use-helm t)

  ;; Shutdown rdm when leaving emacs.
  (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
  (rtags-start-process-unless-running)

  :bind (:map c-mode-base-map
              ("M-." . rtags-find-symbol-at-point)
              ("M-," . rtags-find-references-at-point)
              ("C->" . rtags-location-stack-forward)
              ("C-<" . rtags-location-stack-back)
              ("M-?" . rtags-find-file)
              ("M-;" . rtags-find-file)
              ("C-." . rtags-find-symbol)
              ("C-," . rtags-find-references)
              ("C-;" . rtags-find-virtuals-at-point)
              ("C-c r r" . rtags-rename-symbol)))

(use-package helm-rtags
  :requires helm rtags
  :config
  (setq rtags-display-result-backend 'helm))

(use-package company-rtags
  :requires company rtags
  :config
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t))

(use-package cmake-mode
  :config
  (setq-default cmake-tab-width 4)
  (add-hook 'cmake-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends) 'company-cmake))))

(use-package yasnippet-snippets)

(use-package yasnippet
  :requires yasnippet-snippets
  :config
  (yas-reload-all)
  (add-hook 'c++-mode-hook (lambda ()
                             (yas-minor-mode 1))))

(use-package flycheck-rtags
  :after flycheck rtags
  :config
  ;; ensure that we use only rtags checking
  ;; https://github.com/Andersbakken/rtags#optional-1
  (add-hook 'c-mode-common-hook (lambda ()
                                  (flycheck-mode 1)
                                  (flycheck-select-checker 'rtags)
                                        ;(setq flycheck-global-modes '(cc-mode))
                                  ;; RTags creates more accurate overlays.
                                  (setq-local flycheck-highlighting-mode nil) 
                                  (setq-local flycheck-check-syntax-automatically nil)
                                  ;; Run flycheck 2 seconds after being idle.
                                  (rtags-set-periodic-reparse-timeout 2.0))))

(add-hook 'c++-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         '(company-rtags company-yasnippet company-clang))))

(provide 'cc-setup)
