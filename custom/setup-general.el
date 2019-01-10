(setq gc-cons-threshold 100000000)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(use-package cc-mode
  :init)

(require 'cc-mode)
(define-key c-mode-base-map (kbd "<C-tab>") 'company-complete)
(c-add-style "ana_style"
             '("bsd"
               (c-basic-offset . 4)
               (c-offset-alists
                (case-label . *)
                (innamespace . 0))))
(setq c-default-style "ana_style")

;; company
(use-package company
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends))

;; Package: projectile
(use-package projectile
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

;; Package zygospore
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent))) ; automatically indent when press RET

;; *** Find out what this means ***
(windmove-default-keybindings)

(provide 'setup-general)
