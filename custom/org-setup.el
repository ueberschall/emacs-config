;; Configure org
(use-package org
  :preface
  (defun org-metadown-to-bottom ()
    "Moves the item, row or subtree to the bottom of its parent struct"
    (interactive)
    (condition-case nil
        (while t
          (org-metadown))
      (user-error nil)))
  (defun org-metaup-to-beginning ()
    "Moves the item, row or subtree to the bottom of its parent struct"
    (interactive)
    (condition-case nil
        (while t
          (org-metaup))
      (user-error nil)))

  (defun generate-archive-file-path ()
    "Generate the path to the corresponding archive file for a project name"
    (re-search-backward "\\* \\(.*\\)\\> +:")
    (concat "Archiv/" (downcase (string-replace " " "_" (match-string 1))) ".org::"))

  :custom
  (org-capture-templates
   '(("i" "Input" entry (file "~/Notizen/00_input.org")
      "* %?\n:PROPERTIES:\n:CREATED_AT: %U\n:END:" :empty-lines 1)
     ("p" "Project" entry (file "~/Notizen/01_projects.org")
      "* %^{Project name} %^g\n:PROPERTIES:\n:ID:         %(org-id-new)\n:ARCHIVE:    Archiv/%^{Archive|default}.org::\n:CREATED_AT: %U\n:END:\n\n** Goals\n\n%?\n\n** Resources" :empty-lines 1)
     ("s" "Someday" entry (file "~/Notizen/02_someday_maybe.org")
      "* %? %^{Tags}g\n:PROPERTIES:\n:CREATED_AT: %U\n:END:"
      :empty-lines 1)
     ("t" "Todo" entry (file "~/Notizen/03_next_actions.org")
      "* TODO %? %^{Tags}g\n:PROPERTIES:\n:ID:       %(org-id-new)\n:CREATED_AT: %U\n:END:\n:LOGBOOK:\n:END:"
      :empty-lines 1)))

  :config
  (setq org-directory (expand-file-name "Neue_Notizen" (getenv "HOME")))
  (setq org-link-file-path-type 'relative)
  (setq org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                               (expand-file-name "next_actions.org" org-directory)
                               (expand-file-name "archived_actions.org" org-directory)
                               (expand-file-name "Projects" org-directory)
                               (expand-file-name "Someday_Maybe" org-directory)
                               ))
  (setq org-tags-match-list-sublevels t)
  (setq org-support-shift-select t) ;; Enables region selection with shift and arrow key.
  (setq org-startup-indented t)
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width t)
  (setq org-keep-stored-link-after-insertion t)
  (setq org-fontify-done-headline t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer 'LOGBOOK)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "WAITING(w!)" "PROGRESSING(p!)" "|" "DONE(d!)" "CANCELLED(c!)")))

  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c a" . org-agenda)
              ("C-c c" . org-capture)
              ("<M-s-up>" . org-metaup-to-beginning)
              ("<M-s-down>" . org-metadown-to-bottom))
  :custom-face
  (org-level-4
   ((t (:foreground "DarkRed"))))
  (org-headline-done
   ((t (:foreground "Green" :strike-through "t")))))

;; Show hidden emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-superstar
  :after org
  :config
  ;; Select the bullet list, which shall be in front of all the TODO keywords
  (setq org-superstar-todo-bullet-alist
        '(("TODO" . 9744) ("WAITING" . 8987) ("PROGRESSING" . 8599) ("DONE" . 9745) ("CANCELLED" . 9747)))
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(use-package org-roam
  :after org
  :ensure t
  :custom
  (org-roam-directory (file-truename org-directory))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "Roam/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+options ^:{}\n#+filetags: %^G")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-enable)

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i"   . completion-at-point)))

(provide 'org-setup)
