(define-key c-mode-base-map (kbd "C-c <return>") 'duplicate-line)

(define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
(define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
(define-key c-mode-base-map (kbd "C->") 'rtags-location-stack-forward)
(define-key c-mode-base-map (kbd "C-<") 'rtags-location-stack-back)
(define-key c-mode-base-map (kbd "M-?") 'rtags-find-file)
(define-key c-mode-base-map (kbd "M-;") 'rtags-find-file)
(define-key c-mode-base-map (kbd "C-.") 'rtags-find-symbol)
(define-key c-mode-base-map (kbd "C-,") 'rtags-find-references)
(define-key c-mode-base-map (kbd "C-;") 'rtags-find-virtuals-at-point)
(define-key c-mode-base-map (kbd "C-c r r") 'rtags-rename-symbol)

(define-key c-mode-base-map (kbd "RET") 'cpp-newline)
(define-key c-mode-base-map (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(define-key c-mode-base-map (kbd "M-RET") 'srefactor-refactor-at-point)

;; Clang format
(define-key c-mode-base-map (kbd "C-c f r") 'clang-format-region)
(define-key c-mode-base-map (kbd "C-c f b") 'clang-format-buffer)

;; Doxygen
(define-key c-mode-base-map (kbd "C-c =") 'insert-double-line-comment-seperator)
(define-key c-mode-base-map (kbd "C-c -") 'insert-single-line-comment-seperator)
(define-key c-mode-base-map (kbd "C-c d c") 'extract-and-insert-doxygen-documentation-for-symbol-under-point)

(provide 'setup-cpp-keymap)
