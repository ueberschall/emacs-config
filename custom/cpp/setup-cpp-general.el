(setq gc-cons-threshold 100000000)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1  
(add-hook 'c-initialization-hook (lambda ()
                                   (flycheck-select-checker 'rtags)
                                   ;(setq flycheck-global-modes '(cc-mode))
                                   ;; RTags creates more accurate overlays.
                                   (setq-local flycheck-highlighting-mode nil) 
                                   (setq-local flycheck-check-syntax-automatically nil)
                                   ;; Run flycheck 2 seconds after being idle.
                                   (rtags-set-periodic-reparse-timeout 2.0) 
                                   ;(global-flycheck-mode 1)
                                   ))

(add-hook 'c-mode-common-hook (lambda ()
                                (flycheck-mode 1)))

(add-hook 'c-mode-common-hook 'clang-format-buffer-smart-on-save)

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t

 ;; Show tool tips
 gud-tooltip-mode 1)

(provide 'setup-cpp-general)
