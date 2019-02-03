

(define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
(define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
(define-key c-mode-base-map (kbd "M-#") 'rtags-location-stack-forward)
(define-key c-mode-base-map (kbd "M-'") 'rtags-location-stack-back)
(define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)

(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(define-key c-mode-base-map (kbd "C-x 1") 'clang-format-region)

(provide 'setup-cpp-keymap)
