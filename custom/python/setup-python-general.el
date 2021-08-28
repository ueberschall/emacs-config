(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "python -m "
                         (if buffer-file-name
                             (shell-quote-argument (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))))

(provide 'setup-python-general)
