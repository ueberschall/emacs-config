;; Load some usability functions.
(require 'basic-functions)

;; Set the text in the frame title to be "Emacs <version>".
(setq frame-title-format (concat "Emacs " emacs-version))

;; Disable the Splash screen and set the initial message in the scratch buffer.
(setq inhibit-startup-screen t
      initial-scratch-message ";; Scratch\n\n")

;; Maximize frames.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable tool bar
(tool-bar-mode -1)

;; Adapt mode line.
(setq-default mode-line-format
              '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified
                mode-line-remote mode-line-frame-identification mode-line-buffer-identification " (" mode-name ") "
                mode-line-position (vc-mode vc-mode) " "
                ))

(setq-default mode-line-buffer-identification
              (list (propertize
                     "%12b"
                     'face 'mode-line-buffer-id
                     'help-echo
                     '(format "%s\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                              (buffer-file-name))
                     'mouse-face 'mode-line-highlight
                     'local-map mode-line-buffer-identification-keymap)))

;; Set the default font to Ubuntu-Mono.
(set-face-attribute 'default nil :font "Ubuntu Mono-14")

;; Set the directory for backups of buffers and intervall time between
;; two successive savings
(setq
 backup-by-copying t
 backup-directory-alist '(("." . (expand-file-name ".backups" (getenv "HOME"))))
 auto-save-timeout 180)

;; Activate a couple of useful minor modes.
(global-linum-mode t)  ;; Display row numbers.
(column-number-mode 1) ;; Display column number of point.
(electric-pair-mode 1) ;; Automatic closing of parentheses.
(winner-mode 1) ;; Window actions can be undone.
(global-visual-line-mode 1) ;; Visual line mode is activated globally
(show-paren-mode 1) ;; Display paired parentheses.
(setq show-paren-delay 0) 

;; Make 'yes-or-no' queries easier to confirm.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Settings for tabs and indentations.
(setq-default indent-tabs-mode nil) ;; Use space for indentation instead of tabs.
(setq-default tab-width 4)

;; Loading custom themes is considered safe
(setq custom-safe-themes t)

;;---------------------------------Server Setup-----------------------------------------

(server-start)

;;---------------------------------Package Management-----------------------------------

;; Initialize package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Make sure that 'use-package' is installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package exec-path-from-shell)

;; Configure ace-window
(use-package ace-window
  :bind (("C-c w" . ace-window)))

;; Configure magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;; Configure srefactor-lisp
(use-package srefactor-lisp
  :bind (("M-RET o" . srefactor-lisp-one-line)
         ("M-RET m" . srefactor-lisp-format-sexp)
         ("M-RET d" . srefactor-lisp-format-defun)
         ("M-RET b" . srefactor-lisp-format-buffer)))

;; Use the Cyberpunk-Theme (because it is cool as hell!!)
(use-package cyberpunk-theme
  :config
  (add-hook 'after-init-hook (lambda () (load-theme 'cyberpunk t))))

;; Configure treemacs
(use-package treemacs
  :config
  (treemacs)
  (treemacs-toggle-fixed-width))

;; Configure org
(use-package org
  :config
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

  (setq org-support-shift-select t) ;; Enables region selection with shift and arrow key.
  (setq org-startup-indented t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (add-hook 'org-mode-hook
            (lambda ()
              (org-superstar-mode 1)))
  
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c a" . org-agenda)
              ("C-c c" . org-capture)
              ("<M-s-up>" . org-metaup-to-beginning)
              ("<M-s-down>" . org-metaup-to-bottom)))

;; Configure company
(use-package company
  :init
  (setq-default company-backends
                '((company-files
                   company-keywords
                   company-capf
                   company-yasnippet)
                  (company-dabbrev company-abbrev)))
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.3)
  :bind (("<C-tab>" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  )

;; Configure projectile
(use-package projectile
  :config (projectile-mode 1)
  :bind (("C-?" . projectile-find-other-file)))

;; Configure helm
(use-package helm
  :config
  (setq helm-echo-input-in-header-line t)
  
  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (setq helm-split-window-in-side-p t
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-locate-fuzzy-match t)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (helm-autoresize-mode -1)

  (set-face-attribute 'helm-source-header nil :foreground "dark magenta" :weight 'bold
                      :background "black" :font "Ubuntu Mono-14")
  (set-face-attribute 'helm-selection nil :foreground "white" :background "SpringGreen4")
  (set-face-attribute 'helm-buffer-modified nil :foreground "RosyBrown")

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c r") 'helm-recentf)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
  (global-set-key (kbd "C-c h g") 'helm-google-suggest)
  (global-set-key (kbd "C-c h x") 'helm-register)

  :bind (
         ;; ("C-c h" . helm-command-prefix)
         ;; ("M-x" . helm-M-x)
         ;; ("M-y" . helm-show-kill-ring)
         ;; ("C-x b" . helm-buffers-list)
         ;; ("C-x C-f" . helm-find-files)
         ;; ("C-c r" . helm-recentf)
         ;; ("C-h SPC" . helm-all-mark-rings)
         ;; ("C-c h o" . helm-occur)
         ;; ("C-c h w" . helm-wikipedia-suggest)
         ;; ("C-c h g" . helm-google-suggest)
         ;; ("C-c h x" . helm-register)
         :map help-command
         ("C-f" . helm-apropos)
         ("r" . helm-info-emacs)
         ("C-l" . helm-locate-library)
         :map minibuffer-local-map
         ("M-p" . helm-minibuffer-history)
         ("M-n" . helm-minibuffer-history)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map helm-grep-mode-map
         ("<return>" . helm-grep-mode-jump-other-window)
         ("n" . helm-grep-mode-jump-other-window-forward)
         ("p" . helm-grep-mode-jump-other-window-backward)
         ))

(use-package helm-projectile
  :after helm
  :init
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien))

;; Dired+ has to be downloaded from EmacsWiki.
(let* ((diredplus-file (expand-file-name "dired+/dired+.el" user-emacs-directory))
      (diredplus-dir (file-name-directory diredplus-file)))
  (if (file-exists-p diredplus-file)
    (message "Dired+ does already exist")
  (unless (file-directory-p diredplus-dir)
    (make-directory diredplus-dir))
  (url-copy-file "https://www.emacswiki.org/emacs/download/dired%2b.el"
                 diredplus-file)))

;; Configure Dired+
(use-package dired+
  :init
  (add-to-list 'load-path (expand-file-name "dired+" user-emacs-directory))
  :config (diredp-toggle-find-file-reuse-dir 1)
  :bind (:map dired-mode-map
              ("<C-right>" . windmove-right)
              ("<C-left>" . windmove-left)
              ("<C-up>" . windmove-up)
              ("<C-down>" . windmove-down)))

;; modeline-posn has to be downloaded from the EmacsWiki.
(let* ((modeline-file (expand-file-name "modeline-posn/modeline-posn.el" user-emacs-directory))
      (modeline-dir (file-name-directory modeline-file)))
  (if (file-exists-p modeline-file)
    (message "Modeline-Posn does already exist")
  (unless (file-directory-p modeline-dir)
    (make-directory modeline-dir))
  (url-copy-file "https://www.emacswiki.org/emacs/download/modeline-posn.el"
                 modeline-file)))


;; modeline-posn cannot be configured using use-package.
(add-to-list 'load-path (expand-file-name "modeline-posn" user-emacs-directory))
(require 'modeline-posn)

;;-------------------------------Global key bindings-------------------------------------

(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile-with-prefix-arg)))
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "<C-right>") 'windmove-right)
(global-set-key (kbd "<C-left>") 'windmove-left)
(global-set-key (kbd "<C-up>") 'windmove-up)
(global-set-key (kbd "<C-down>") 'windmove-down)
(global-set-key (kbd "C-c <return>") 'duplicate-line)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package shell-mode)

;; Shell-mode
(define-key shell-mode-map (kbd "<C-right>") 'windmove-right)
(define-key shell-mode-map (kbd "<C-left>") 'windmove-left)
(define-key shell-mode-map (kbd "<C-up>") 'windmove-up)
(define-key shell-mode-map (kbd "<C-down>") 'windmove-down)

(provide 'basic-setup)
