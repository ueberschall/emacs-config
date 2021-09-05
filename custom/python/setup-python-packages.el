;; (install-necessary-packages '(pylint
;;                               realgud
;;                               py-autopep8
;;                               elpy))

(use-package pylint)

(use-package realgud)

(use-package py-autopep8)

(use-package flycheck)

(use-package elpy
  :after flycheck
  :init
  (query-and-store-pyvenv-path (expand-file-name ".pyvenv_path" "~/.emacs.d"))
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
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  
  (elpy-enable)
  (setq pyvenvPath (load-pyvenv-path-from-file (expand-file-name ".pyvenv_path" "~/.emacs.d")))
  (when pyvenvPath
    (pyvenv-activate pyvenvPath))
  (setq elpy-rpc-virtualenv-path 'current))
  

(provide 'setup-python-packages)
