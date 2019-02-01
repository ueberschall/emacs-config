;; *** Speichern des Desktops (Quelle: https://www.emacswiki.org/emacs/Desktop)
(setq desktop-load-locked-desktop t)

(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

(add-hook 'desktop-after-read-hook 'delete-desktop)
(add-hook 'kill-emacs-hook 'session-save)

(provide 'setup-desktop)
