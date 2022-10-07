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
  :config
  (setq org-directory (expand-file-name "Neue_Notizen" (getenv "HOME")))
  (setq org-link-file-path-type 'relative)
  (setq org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                               (expand-file-name "next_actions.org" org-directory)
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
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         :map org-mode-map
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
  :ensure t
  :config
  (setq org-roam-directory org-directory)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
   '(("i" "Inbox" plain
      "* ${title}\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n\n%?"
      :target (file+head "inbox.org" "#+title: 0 Inbox\n#+options: ^:{}")
      :unnarrowed t :empty-lines 1 :jump-to-captured nil)
     ("t" "Todo" plain
      "* TODO ${title}\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n\n%?"
      :target (file+head "next_actions.org" "#+title: 1 Next Actions\n#+options ^:{}") :unnarrowed t :empty-lines 1 :jump-to-captured nil)
      ("w" "Recurring Todo" plain
      "* TODO ${title} :recurring:\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n\n%?"
      :target (file+head "next_actions.org" "#+title: 1 Next Actions\n#+options ^:{}")
      :unnarrowed t :empty-lines 1)
     ("p" "Project" plain
      "* Ziele\n\n%?\n\n* Aufgaben :projects:\n\n"
      :target (file+head "Projects/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+options ^:{}\n#+filetags: :projects:")
      :unnarrowed t)
     ("s" "Someday maybe" plain
      "%?"
      :target (file+head "Someday_Maybe/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+options ^:{}\n#+filetags: :someday_maybe:")
      :unnarrowed t)
     ("r" "Reference" plain
      "%?"
      :target (file+head "References/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+options ^:{}\n#+filetags: :references:")
      :unnarrowed t)
     ("m" "Merge Request Review" plain
      "* Link\n\n %?\n\n* Aufgaben\n\n** TODO Änderungen überprüfen\n\n** TODO Kommentare diskutieren\n\n** TODO Approven"
      :target (file+head "Projects/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+options ^:{}\n#+filetags: :projects:mr_review:")
      :unnarrowed t)
     ("j" "Jira Story" plain
      "* Link\n\n %?\n\n* Aufgaben\n\n"
      :target (file+head "Projects/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+options ^:{}\n#+filetags: :projects:jira:")
      :unnarrowed t)))
  (org-roam-db-autosync-enable)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i" . completion-at-point)))

(provide 'org-setup)
