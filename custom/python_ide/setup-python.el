;; python_ide.el

;; *** matplotlib in Elpy ***
;; Um in Elpy matplotlib-Figures auftauchen zu lassen, benutze am besten die Jupyter-Konsole.
;; Führe darin den Befehl "%matplotlib" aus. Dann werden Plots auch ohne den
;; "matplotlib.pyplot.show()"-Befehl einfach gerendert. Ausführen von "matplotlib.pyplot.ioff()"
;; sorgt dafür, dass das nicht mehr so ist.
;;
;; *** Debuggen mit RealGUD ***
;; Um zu mit RealGUD zu debuggen ist es notwendig den Befehl
;; "realgud:pdb" zu aktivieren und dann das Kommando
;; "python -m pdb <scriptname>.py" zu aktivieren.

(require 'setup-python-package)


;; (setq flake8-config-file (expand-file-name ".flake8" pyvenv-name))
;; (if (file-exists-p flake8-config-file)
;;     (setq flycheck-flake8rc flake8-config-file))

(flymake-mode-off)
(flycheck-mode -1)

(provide 'setup-python)
