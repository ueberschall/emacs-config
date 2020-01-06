;; *** Installiere fehlende Packete ***
;; rtags und alle damit verbundenen Packete (helm-rtags, company-rtags usw)
;; werden nicht aus den Emacs-Packetquellen installiert, sondern manuell
(install-necessary-packages '(zygospore
                              clang-format))

(use-package cc-mode
  :init
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
                             (c-set-offset 'innamespace 0))))

(use-package rtags
  :config
  (setq rtags-path "/usr/local/bin")
  (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
  (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

  (setq rtags-use-helm t)

  ;; Shutdown rdm when leaving emacs.
  (add-hook 'kill-emacs-hook 'rtags-quit-rdm)

  (use-package helm-rtags
  :requires helm rtags
  :config
  (setq rtags-display-result-backend 'helm))

  (use-package company-rtags
  :requires company rtags
  :config
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (push 'company-rtags company-backends)
  (setq rtags-completions-enabled t))

  (rtags-start-process-unless-running))

(require 'flycheck-rtags)

;; (require 'rtags)
;; (cmake-ide-setup)

(use-package zygospore)

(use-package clang-format
  :config
  (setq clang-format-style-option "/btool/SDK/src/ref/common/.clang-format"))

(provide 'setup-cpp-packages)
