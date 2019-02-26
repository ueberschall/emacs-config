(use-package ein)

(use-package pylint)

(use-package realgud)

(use-package py-autopep8)

(use-package flycheck)

(use-package elpy
  :init
  (progn
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))
  :config
  (progn
    (setq python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt"
          python-shell-prompt-detect-failure-warning nil)
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "jupyter")
    (elpy-enable)))

(provide 'setup-python-package)
