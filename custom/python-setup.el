(use-package python
  :mode ("\\.py$" . python-mode)
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (concat "python -m "
                           (if buffer-file-name
                               (shell-quote-argument (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))))
  (add-hook 'inferior-python-mode-hook (lambda ()
                                         (linum-mode -1))))

(use-package pylint
  :after elpy)

(use-package py-autopep8
  :after python)

(use-package elpy
  :after (python flycheck)
  :hook (python-mode . elpy-mode)
  :preface
  (setq pyvenv-path-file (expand-file-name ".pyvenv-path" user-emacs-directory))
  (defun query-and-load-and-store-pyvenv-path-file ()
    "Query from user the desired pyvenv path, than load and store it."
    (interactive)
    (setq pyvenv-dir-p nil)
    (while (not pyvenv-dir-p)
      (setq pyvenv-dir (read-directory-name "Pyvenv directory path: " (read-path-from-file pyvenv-path-file) nil t))
      (setq pyvenv-dir-p (file-exists-p
                          (expand-file-name "bin/activate" pyvenv-dir)))
      (when (not pyvenv-dir-p)
        (unless (y-or-n-p (concat pyvenv-dir " is not a Pyvenv directory! Select another directory? "))
          (setq pyvenv-dir nil)
          (setq pyvenv-dir-p t))))
    (when pyvenv-dir
      (pyvenv-activate pyvenv-dir)
      (set-buffer (find-file-literally pyvenv-path-file))
      (erase-buffer)
      (insert pyvenv-dir)
      (save-buffer)
      (kill-buffer)))
  :init
  (elpy-enable)
  (add-hook 'elpy-mode-hook (lambda ()
                              (add-hook 'before-save-hook
                                        'elpy-format-code nil t)))
  (add-hook 'elpy-mode-hook (lambda () (flycheck-mode 1)))
  (add-hook 'elpy-mode-hook (lambda () (py-autopep8-mode 1)))
  :config
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (if (not (file-exists-p pyvenv-path-file))
      (query-and-load-and-store-pyvenv-path-file)
    (setq pyvenv-path (read-path-from-file pyvenv-path-file))
    (when (and pyvenv-path (file-exists-p
                            (expand-file-name "bin/activate" pyvenv-path)))
      (pyvenv-activate pyvenv-path)))
  (setq elpy-rpc-virtualenv-path 'current)
  :bind (:map elpy-mode-map
              ("<C-right>" . windmove-right)
              ("<C-left>" . windmove-left)
              ("<C-up>" . windmove-up)
              ("<C-down>" . windmove-down)
              ("C-c p p" . query-and-load-and-store-pyvenv-path-file)))

(use-package ein
  :after python
  :custom
  (ein:output-area-inlined-images t))

(use-package realgud
  :after elpy
  :bind (:map elpy-mode-map
              ("C-c p d" . realgud:pdb)))

(provide 'python-setup)
