(define-key python-mode-map (kbd "C-c c") 'comment-region)
(define-key python-mode-map (kbd "C-c M-c") 'uncomment-region)
(define-key python-mode-map (kbd "C-c f") 'elpy-format-code)

(provide 'setup-python-keymap)
