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

 ;; Show tool tips
 gud-tooltip-mode 1

 ;; Disable company mode for GDB
 company-global-modes '(not gud-mode))

(provide 'setup-cpp-general)
