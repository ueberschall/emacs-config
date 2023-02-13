;; Ensure that the my/org-roam-filter-by-tag function is working

;; -*- lexical-binding: t; -*-

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

  (defun my/skip-recurring-todos ()
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (not (re-search-forward ":wiederkehrend:" subtree-end t))
          nil     ; tag found, skip it
        subtree-end)))

  (defun my/archive-projects-org-file ()
    (interactive)
    (let ((current-file (buffer-file-name)))
      (goto-char (point-min))
      (if (re-search-forward "#\\+FILETAGS:.*:projekte.*:" nil t)
          (insert (concat "archiviert:\n#+ARCHIVED_AT: " (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (display-warning "This file is not a projects org file!"))
      (save-buffer)
      (kill-buffer)
      (setq org-agenda-files (delete current-file org-agenda-files))))

  (defun my/archive-done-todos ()
    "Archive all done TODO items in the current org-mode buffer."
    (interactive)
    (save-excursion
      (find-file (expand-file-name "next_actions.org" org-directory))
      (let ((org-archive-location (expand-file-name (concat (format-time-string "%Y-%m-%d-%H-%M") ".org::") (expand-file-name "Archiv" org-directory))))
        (org-map-entries
         (lambda ()
           (org-archive-subtree)
           (setq org-map-continue-from (outline-previous-heading)))
         "/DONE" 'file))
      (save-buffer)))

  :init
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'org-agenda-mode-hook (lambda () (linum-mode -1)))
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
  :custom
  (org-directory (expand-file-name "Notizen" (getenv "HOME")))
  (org-link-file-path-type 'relative)
  (org-agenda-files (list (expand-file-name "next_actions.org" org-directory)))
  (org-use-sub-superscripts "{}")
  (org-tags-match-list-sublevels t)
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
  (org-agenda-prefix-format
   '((todo . " - ")
     (agenda . "%t: ")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
  (org-agenda-custom-commands
   '(("A" "Persönliche Agenda View"
      ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")
                 (org-agenda-remove-tags t)))
       (agenda "" ((org-agenda-span 7)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-remove-tags t)
                   (org-agenda-time-grid nil)
                   (org-agenda-entry-types '(:timestamp :sexp :deadline :scheduled))))
       (alltodo ""
                ((org-agenda-files `(,(expand-file-name "next_actions.org" org-directory)))
                 (org-agenda-skip-function 'my/skip-recurring-todos)))
       (tags-todo "projekte" ((org-agenda-hide-tags-regexp "projekte")))))
     ("D" "Daily" search ""
      ((org-agenda-files `(,(expand-file-name (concat (format-time-string "%Y-%m-%d") ".org")
                                              (expand-file-name "Dailies" org-directory))))))))
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
    (my/org-roam-list-notes-by-tag "projekte"))

  (defun my/org-roam-archived-note-list ()
    (my/org-roam-list-notes-by-tag "archiviert"))

  (defun my/org-roam-disable-db-sync (&rest ignore)
    "Disable the org data base sync"
    (interactive)
    (org-roam-db-autosync-disable))

  (defun my/org-roam-enable-db-sync (&rest ignore)
    "Enable the org data base sync"
    (interactive)
    (org-roam-db-autosync-enable))

  (defun my/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))

  (defun my/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil nil
     (lambda (node)
       (member "projekte" (org-roam-node-tags node)))
     nil
     :templates
     '(("p" "Projekt" plain
        "\n* Ziel\n\n%?\n\n* Aufgaben :projekte:\n\n"
        :target (file+head "Projekte/${slug}.org" "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: :projekte:")
        :unnarrowed t))))
  :init (my/archive-done-todos)
  :hook (org-mode . org-roam-db-autosync-enable)
  :custom
  (org-roam-directory org-directory)
  (org-roam-file-exclude-regexp
      (concat "^" (expand-file-name org-roam-directory) "/Archiv/\\|.*\\.org\\.gpg"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
        '(("i" "Inbox" plain
           "* ${title}\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "inbox.org" "#+TITLE: 0 Inbox")
           :unnarrowed t)
          ("t" "Todo" plain
           "* TODO ${title}\n:PROPERTIES:\n:ID:        %(org-id-new)\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "next_actions.org" "#+TITLE: 1 Next Actions") :unnarrowed t)
          ("w" "Wiederkehrendes Todo" plain
           "* TODO ${title} :wiederkehrend:\n:PROPERTIES:\n:ID:        %(org-id-new)\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "next_actions.org" "#+TITLE: 1 Next Actions")
           :unnarrowed t)
          ("s" "Someday Maybe" plain
           "* TODO ${title} :someday_maybe:\n:PROPERTIES:\n:ID:        %(org-id-new)\n:CREATED_AT: %U\n:END:\n\n%?"
           :target (file+head "someday_maybe.org" "#+TITLE: 2 Someday Maybe\n#+FILETAGS: :someday_maybe:")
           :unnarrowed t)
          ("p" "Projekt" plain
           "\n* Ziel\n\n%?\n\n* Aufgaben :projekte:\n\n"
           :target (file+head "Projekte/${slug}.org" "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: :projekte:")
           :unnarrowed t)
          ("r" "Referenz" plain
           "%?"
           :target (file+head "Referenzen/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :referenzen:")
           :unnarrowed t)
          ("m" "Merge Request Review" plain
           "* Link\n\n %?\n\n* Aufgaben\n\n** TODO Anpassungen reviewen\n\n** TODO Kommentare diskutieren\n\n** TODO Approven"
           :target (file+head "Projekte/mr-${slug}.org" "#+TITLE: ${title}\n#+CATEGORY: Merge Request\n#+FILETAGS: :projekte:mr_review:")
           :unnarrowed t)
          ("k" "Kochrezept" plain
           "%?\n\n* Zutaten :kochen:\n\n* Zubereitung"
           :target (file+head "Referenzen/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+CATEGORY: Kochrezepte\n#+FILETAGS: :referenzen:kochen:")
           :unnarrowed t)
          ("b" "Backrezept" plain
           "%?\n\n* Zutaten :backen:\n\n* Zubereitung"
           :target (file+head "Referenzen/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+CATEGORY: Backrezepte\n#+FILETAGS: :referenzen:backen:")
           :unnarrowed t)))
  (org-roam-dailies-directory org-roam-directory)
  (org-roam-dailies-capture-templates
   '(("d" "Daily" plain
      "%?"
      :target (file+head "Dailies/%<%Y-%m-%d>.org" "#+TITLE: Daily-Eintrag %<%Y-%m-%d>\n#+CATEGORY: Daily\n#+FILETAGS: :daily:\n#+begin: clocktable :scope agenda :block %<%Y-%m-%d> :link t\n|Headline   | Time |\n|------------+------|\n| *Total time* | *0:00* |\n#+end: clocktable\n") :unnarrowed t)
     ("t" "Tagebucheintrag" plain "%?" :target
      (file+head "Tagebuch/%<%Y-%m-%d>.org.gpg" "#+TITLE: Tagebucheintrag %<%Y-%m-%d>\n#+CATEGORY: Diary\n#+FILETAGS: :diary:") :unnarrowed t)))
  :config
  (setq org-agenda-files
        (append org-agenda-files

                (cl-set-difference
                 org-agenda-files
                 (cl-set-difference (my/org-roam-project-note-list) (my/org-roam-archived-note-list) :test 'string=)
                 :test 'string=)))
  (my/archive-done-todos)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n p" . my/org-roam-find-project)
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
