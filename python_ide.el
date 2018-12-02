;; python_ide.el

;; *** matplotlib in Elpy ***
;; Um in Elpy matplotlib-Figures auftauchen zu lassen, benutze am besten die Jupyter-Konsole.
;; Führe darin den Befehl "%matplotlib" aus. Dann werden Plots auch ohne den
;; "matplotlib.pyplot.show()"-Befehl einfach gerendert. Ausführen von "matplotlib.pyplot.ioff()"
;; sorgt dafür, dass das nicht mehr so ist.

(elpy-enable)

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(pyvenv-activate "/media/zufall/inter/Python/play_env")
(setq flycheck-flake8rc "/media/zufall/inter/Python/play_env/.flake8")
