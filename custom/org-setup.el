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
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  (setq org-directory (expand-file-name "Notizen" (getenv "HOME")))
  (setq org-link-file-path-type 'relative)
  (setq org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                               (expand-file-name "next_actions.org" org-directory)))
  (setq org-use-sub-superscripts "{}")
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
  :after org
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
  :config
  (setq org-roam-directory org-directory)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
        '(("i" "Inbox" plain
           "* ${title}\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "inbox.org" "#+title: 0 Inbox")
           :unnarrowed t :empty-lines 1)
          ("t" "Todo" plain
           "* TODO ${title}\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "next_actions.org" "#+title: 1 Next Actions") :unnarrowed t :empty-lines 1)
          ("w" "Recurring Todo" plain
           "* TODO ${title} :recurring:\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "next_actions.org" "#+title: 1 Next Actions")
           :unnarrowed t :empty-lines 1)
          ("s" "Someday maybe" plain
           "* Someday Maybe: ${title} :someday_maybe:\n:PROPERTIES:\n:ID:        %(org-id-new)\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "someday_maybe.org" "#+title: 2 Someday Maybe\n#+filetags: :someday_maybe:")
           :unnarrowed t :empty-lines 1)
          ("p" "Project" plain
           "* Beschreibung :projects:\n\n** Ziele\n\n%?\n\n* Aufgaben :projects:\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: 3 Project: ${title}\n#+category: \n#+filetags: :projects:")
           :unnarrowed t)
          ("r" "Reference" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: 4 Reference: ${title}\n#+category: \n#+filetags: :references:")
           :unnarrowed t)
          ("m" "Merge Request Review" plain
           "* Link\n\n %?\n\n* Aufgaben\n\n** TODO Änderungen überprüfen\n\n** TODO Kommentare diskutieren\n\n** TODO Approven"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: Merge Request\n#+filetags: :mr_review:")
           :unnarrowed t)
          ("j" "Jira Story" plain
           "* Link\n\n %?\n\n* Aufgaben\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: Jira\n#+filetags: :jira:")
           :unnarrowed t)))
  (setq org-roam-dailies-directory "Diary/")
  (setq org-roam-dailies-capture-templates
        '(("d" "Diary" entry "%?" :target
           (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
") :unnarrowed t)))
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

;; Filtering of the Org Roam nodes with respect to a tags
;;---------------------------------------------------------------
;; (defun my/org-roam-filter-by-tag (tag-name)
;;   (lambda (node)
;;     (member tag-name (org-roam-node-tags node))))

;; (defun my/org-roam-list-notes-by-tag (tag-name)
;;   (mapcar #'org-roam-node-file
;;           (seq-filter
;;            (my/org-roam-filter-by-tag tag-name)
;;            (org-roam-node-list))))

;; (defun my/org-roam-refresh-agenda-list ()
;;   (interactive)
;;   (setq org-agenda-files (my/org-roam-list-notes-by-tag "projects")))

;;------------------------------------------------------------------

(provide 'org-setup)
