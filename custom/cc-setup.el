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
                             (c-set-offset 'innamespace 0))))

(use-package clang-format
  :config
  (add-hook 'before-save-hook (lambda ()
                                (when (and
                                       (derived-mode-p 'cc-mode)
                                       (file-exists-p (expand-file-name ".clang-format" (projectile-project-root))))
                                  (clang-format-buffer)))))

(use-package rtags
  :if (executable-find "rdm")
  :load-path "/usr/local/share/emacs/site-lisp/rtags"
  :config
  (setq rtags-path "/usr/local/bin"
        rtags-use-helm t)

  ;; Shutdown rdm when leaving emacs.
  (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
  (rtags-start-process-unless-running))

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

(use-package yasnippet
  :requires yasnippet-snippets
  :config
  (yas-reload-all)
  (add-hook 'c++-mode-hook (lambda ()
                             (yas-minor-mode 1))))

(use-package flycheck)

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
                                  (rtags-set-periodic-reparse-timeout 2.0) 
                                        ;(global-flycheck-mode 1)
                                  )))

(use-package zygospore)

(add-hook 'c++-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         '(company-rtags company-yasnippet company-clang))))

(provide 'cc-setup)
