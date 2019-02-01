;; *** Lege das Kompilier-Kommando auf <f5> ***
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; *** Das Fenster kann jetzt mit C-c w gewechselt werden ***
(global-set-key (kbd "C-c w") 'ace-window)

(provide 'setup-basic-keymap)
