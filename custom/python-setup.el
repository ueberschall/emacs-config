;;-----------------------------Functions---------------------------

(defun load-pyvenv-path-from-file (file)
  "Activate the pyvenv which path is specified in file"
  (when (file-exists-p file)
    (save-excursion
      (let ((buf (find-file-literally file)))
        (set-buffer buf)
        (setq path (buffer-string))
        (kill-buffer)
        (replace-regexp-in-string "\n$" "" path)))))

(defun query-and-store-pyvenv-path (file)
  "Query from user the desired pyvenv path"
  (when (y-or-n-p "Do you want to load a Pyvenv? ")
    (setq pyvenv-dir-p nil)
    (while (not pyvenv-dir-p)
      (setq pyvenv-dir (read-directory-name "Pyvenv directory path: " (load-pyvenv-path-from-file file) nil t))
      (setq pyvenv-dir-p (file-exists-p
                          (expand-file-name "bin/activate" pyvenv-dir)))
      (when (not pyvenv-dir-p)
        (unless (y-or-n-p (concat pyvenv-dir " is not a Pyvenv directory! Select another directory? "))
          (setq pyvenv-dir "")
          (setq pyvenv-dir-p t))))
    (let ((buf (find-file-literally file)))
      (set-buffer buf)
      (erase-buffer)
      (insert pyvenv-dir)
      (save-buffer)
      (kill-buffer))))

;;-------------------------------Packages------------------------

(use-package python
  :config
  (add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "python -m "
                         (if buffer-file-name
                             (shell-quote-argument (file-name-sans-extension (file-name-nondirectory buffer-file-name)))))))))

(use-package pylint)

(use-package realgud)

(use-package py-autopep8)

(use-package elpy
  :after flycheck
  :init
  ;;(query-and-store-pyvenv-path (expand-file-name ".pyvenv_path" "~/.emacs.d"))
  :config
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")

  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))

  (add-hook 'elpy-mode-hook (lambda ()
                              (add-hook 'before-save-hook
                                        'elpy-format-code nil t)))
  (add-hook 'elpy-mode-hook (lambda () (flycheck-mode 1)))
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

  (elpy-enable)
  ;; (setq pyvenvPath (load-pyvenv-path-from-file (expand-file-name ".pyvenv_path" user-emacs-directory)))
  ;; (when pyvenvPath
  ;;   (pyvenv-activate pyvenvPath))
  (setq elpy-rpc-virtualenv-path 'current)

  :bind (:map python-mode-map
              ("C-c f" . elpy-format-mode)))

(provide 'python-setup)
