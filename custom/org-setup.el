;; Configure org
(use-package org
  :mode ("\\.org$" . org-mode)
  :demand t
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
  :init
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'org-clock-in-hook (lambda ()
                                 (save-excursion
                                   (org-back-to-heading t)
                                   (let* ((element (org-element-at-point))
                                          (todo-state (org-element-property :todo-keyword element)))
                                     (unless (string= (substring-no-properties todo-state) "PROGRESSING")
                                       (org-todo "PROGRESSING"))))))
  (add-hook 'org-clock-out-hook (lambda ()
                                  (save-excursion
                                    (org-roam-dailies-goto-today "d")
                                    (beginning-of-buffer)
                                    (search-forward "#+begin: clocktable")
                                    (org-dblock-update)
                                    (save-buffer)
                                    (kill-buffer))))
  (add-hook 'org-after-todo-state-change-hook (lambda ()
                                                (let ((active-buffer (file-name-nondirectory (buffer-file-name))))
                                                  (when (and (or (string= org-state "DONE") (string= org-state "CANCELLED"))
                                                             (string= active-buffer "next_actions.org"))
                                                    (org-archive-subtree)
                                                    (save-buffer)))))
  :custom
  (org-directory (expand-file-name "2_Notizen" (getenv "HOME")))
  (org-link-file-path-type 'relative)
  (org-agenda-files (list (expand-file-name "next_actions.org" org-directory)))
  (org-use-sub-superscripts "{}")
  (org-tags-match-list-sublevels t)
  ;; Enable region selection with shift and arrow key.
  (org-support-shift-select t)
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width t)
  (org-keep-stored-link-after-insertion t)
  (org-fontify-todo-headline t)
  (org-fontify-done-headline t)
  (org-log-done 'time)
  (org-log-into-drawer 'CLOCKS)
  (org-todo-keywords
   '((sequence "TODO(t!)" "WAITING(w!)" "PROGRESSING(p!)" "|" "DONE(d!)" "CANCELLED(c!)")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (emacs-lisp . t) (ein . t)))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("<M-s-up>" . org-metaup-to-beginning)
         ("<M-s-down>" . org-metadown-to-bottom))
  :custom-face
  (org-level-4
   ((t (:foreground "DarkRed"))))
  (org-headline-todo
   ((t (:foreground "#ffa500"))))
  (org-headline-done
   ((t (:foreground "Green" :strike-through "t")))))

;; Show hidden emphasis markers
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks nil))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; Select the custom UTF-8 symbols for the TODO keywords
  (org-superstar-todo-bullet-alist
   '(("TODO" . 9744) ("WAITING" . 8987) ("PROGRESSING" . 8599) ("DONE" . 9745) ("CANCELLED" . 9747)))
  (org-superstar-special-todo-items t))

(use-package org-roam
  :after org
  :preface
  (defun my/org-roam-filter-by-tag (tag-name)
    "Return function which tells whether NODE contains TAG-NAME as tag."
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))
  (defun my/org-roam-list-notes-by-tag (tag-name)
    "Determine the file names of the org-roam-nodes which contain TAG-NAME as tag."
    (mapcar #'org-roam-node-file
            (seq-filter
             (my/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))
  (defun my/org-roam-project-note-list ()
    (my/org-roam-list-notes-by-tag "projects"))
  (defun my/org-roam-disable-db-sync (&rest ignore)
    "Disable the org data base sync"
    (interactive)
    (org-roam-db-autosync-disable))
  (defun my/org-roam-enable-db-sync (&rest ignore)
    "Enable the org data base sync"
    (interactive)
    (org-roam-db-autosync-enable))
  :init
  ;; These two advice functions are the workaround for the error which
  ;; causes the org-roam-database to loose all its entries when
  ;; "org-roam-dailies-capture-today" is invoked.
  (advice-add 'org-roam-dailies-capture-today :before #'my/org-roam-disable-db-sync)
  (advice-add 'org-roam-dailies-capture-today :after #'my/org-roam-enable-db-sync)
  :hook (org-mode . org-roam-db-autosync-enable)
  :custom
  (org-roam-directory org-directory)
  (org-roam-file-exclude-regexp
      (concat "^" (expand-file-name org-roam-directory) "/Archive/\\|.*\\.org\\.gpg"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
        '(("i" "Inbox" plain
           "* ${title}\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "inbox.org" "#+title: 0 Inbox")
           :unnarrowed t)
          ("t" "Todo" plain
           "* TODO ${title}\n:PROPERTIES:\n:ID:        %(org-id-new)\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "next_actions.org" "#+title: 1 Next Actions") :unnarrowed t)
          ("w" "Recurring Todo" plain
           "* TODO ${title} :recurring:\n:PROPERTIES:\n:ID:        %(org-id-new)\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "next_actions.org" "#+title: 1 Next Actions")
           :unnarrowed t)
          ("s" "Someday maybe" plain
           "* ${title} :someday_maybe:\n:PROPERTIES:\n:ID:        %(org-id-new)\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "someday_maybe.org" "#+title: 2 Someday Maybe\n#+filetags: :someday_maybe:")
           :unnarrowed t)
          ("p" "Project" plain
           "%?\n\n* Aufgaben :projects:\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: \n#+filetags: :projects:")
           :unnarrowed t)
          ("r" "Reference" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :references:")
           :unnarrowed t)
          ("m" "Merge Request Review" plain
           "* Link\n\n %?\n\n* Aufgaben\n\n** TODO Anpassungen reviewen\n\n** TODO Kommentare diskutieren\n\n** TODO Approven"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: Merge Request\n#+filetags: :mr_review:")
           :unnarrowed t)
          ("k" "Kochrezept" plain
           "%?\n\n* Zutaten :kochen:\n\n* Zubereitung"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :kochen:")
           :unnarrowed t)
          ("b" "Backrezept" plain
           "%?\n\n* Zutaten :backen:\n\n* Zubereitung"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :backen:")
           :unnarrowed t)))
  (org-roam-dailies-directory "Dailies_Diary/")
  (org-roam-dailies-capture-templates
   '(("d" "Daily" plain
      "%?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: Daily-Eintrag %<%Y-%m-%d>\n#+category: Daily\n#+filetags: :daily:\n#+begin: clocktable :scope agenda :block %<%Y-%m-%d> :link t\n|Headline   | Time |\n|------------+------|\n| *Total time* | *0:00* |\n#+end: clocktable\n") :unnarrowed t)
     ("t" "Diary" plain "%?" :target
      (file+head "%<%Y-%m-%d>.org.gpg" "#+title: Tagebucheintrag %<%Y-%m-%d>\n#+category: Diary\n#+filetags: :diary:") :unnarrowed t :kill-buffer t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c C-o" . org-open-at-point)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i" . completion-at-point)))

;; Filtering of the Org Roam nodes with respect to a tags
;;---------------------------------------------------------------

;;------------------------------------------------------------------

(provide 'org-setup)
