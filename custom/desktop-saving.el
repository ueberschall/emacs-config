;; Save the desktop (see https://www.emacswiki.org/emacs/Desktop)
(setq desktop-load-locked-desktop t)

(setq desktop-path (list user-emacs-directory))
(setq desktop-dirname user-emacs-directory)
(setq desktop-base-file-name "emacs-desktop")

(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	      (desktop-save-in-desktop-dir)
	    (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

(defun delete-desktop ()
  "Delete desktop file without setting desktop-dirname to nil"
  (interactive)
  (setq desktop-dirname-tmp desktop-dirname)
  (desktop-remove)
  (setq desktop-dirname desktop-dirname-tmp))

(defun saved-session ()
  (file-exists-p (expand-file-name desktop-base-file-name desktop-dirname)))

(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

(defun load-desktop (base-name)
  "Load the desktop file 'base-name'"
  (setq desktop-base-file-name base-name)
  (if (saved-session)
	  (if (y-or-n-p "Restore desktop? ")
		  (session-restore))))


(add-hook 'after-init-hook (lambda () (load-desktop desktop-base-file-name)))
(add-hook 'desktop-after-read-hook 'delete-desktop)

;; When Emacs is killed, keep only the 30 most recent buffers and kill the rest.
;; Furthermore, save the current desktop session.
(add-hook 'kill-emacs-hook (lambda ()
                             (save-and-kill-outdated-buffers 30)
                             (session-save)))

(provide 'desktop-saving)
