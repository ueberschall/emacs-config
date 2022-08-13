;; Configure C/C++-Mode
(use-package cc-mode
  :config
  (setq gc-cons-threshold 100000000
        gdb-many-windows t
        gdb-show-main t
        gud-tooltip-mode 1
        company-global-modes '(not gud-mode)))

(use-package cmake-mode
  )

(provide 'cc-setup)
