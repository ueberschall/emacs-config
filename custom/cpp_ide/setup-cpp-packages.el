(use-package cc-mode
  :init
  (progn
    (c-add-style "ana_style"
                 '("bsd"
                   (c-basic-offset . 4)
                   (c-offset-alists
                    (case-label . *)
                    (innamespace . 0))))

    (setq c-default-style "ana_style")
    (add-hook 'c-mode-common-hook (lambda ()
                                    (c-set-style "ana_style")))
    (add-hook 'c++-mode-hook (lambda ()
                               (c-set-offset 'innamespace 0)))))

;; (use-package semantic
;;   :init
;;   (progn
;;     (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;     (semantic-mode 1)))

(use-package rtags
  :config
  (progn
    (setq rtags-path "~/RTags/rtags/bin")
    (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
    (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

    (setq rtags-use-helm t)

    ;; Shutdown rdm when leaving emacs.
    (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
    (rtags-start-process-unless-running)))

(use-package helm-rtags
  :requires helm rtags
  :config
  (progn
    (setq rtags-display-result-backend 'helm)))

(use-package company-rtags
  :requires company rtags
  :config
  (progn
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (push 'company-rtags company-backends)
  (setq rtags-completions-enabled t)))

(require 'flycheck-rtags)

(use-package zygospore)

(use-package srefactor)

(provide 'setup-cpp-packages)
