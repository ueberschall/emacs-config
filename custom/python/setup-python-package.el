(install-necessary-packages '(pylint
                              realgud
                              py-autopep8
                              elpy))

(use-package pylint)

(use-package realgud)

(use-package py-autopep8)

(use-package flycheck)

(use-package elpy
  :after flycheck
  :config
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  
  (elpy-enable))

(provide 'setup-python-package)
