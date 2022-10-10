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
(set-face-attribute 'default nil :font "Ubuntu Mono-16")

;; Set the directory for backups of buffers and intervall time between
;; two successive savings
(setq
 backup-by-copying t
 backup-directory-alist `(("." . ,(expand-file-name ".backups" (getenv "HOME"))))
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

;;---------------------------------Package Management-----------------------------------

;; Initialize package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(unless (y-or-n-p "Skip packages refresh")
  ;; Make sure that 'use-package' is installed.
  (unless (package-installed-p 'use-package)
    (package-initialize)
    (package-refresh-contents)
    (package-install 'use-package))
  ;; Make sure that every package which is loaded by use-package is actually installed.
  (setq use-package-always-ensure t)
  (setq use-package-always-pin "melpa"))

(require 'use-package)

(use-package exec-path-from-shell)

;; Configure ace-window
(use-package ace-window
  :bind (("C-c w" . ace-window)))

;; Configure magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;; Configure srefactor-lisp
(use-package srefactor
  :bind (("M-RET o" . srefactor-lisp-one-line)
         ("M-RET m" . srefactor-lisp-format-sexp)
         ("M-RET d" . srefactor-lisp-format-defun)
         ("M-RET b" . srefactor-lisp-format-buffer)))

;; Use the Cyberpunk-Theme (because it is cool as hell!!)
(use-package cyberpunk-theme
  :config
  (add-hook 'after-init-hook (lambda () (load-theme 'cyberpunk t))))

;; Configure treemacs
(use-package treemacs)

;; Configure company
(use-package company
  :init
  (setq-default company-backends
                '(company-files
                  (company-capf company-dabbrev)))

  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-limit 10
        company-tooltip-minimum 10
        company-idle-delay 0
        company-dabbrev-ignore-case nil
        company-dabbrev-other-buffers 'all
        company-dabbrev-downcase nil
        company-files-exclusions '(".git/"))

  :hook ((prog-mode . company-mode)
         (text-mode . company-mode))

  :bind (("<C-tab>" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))

  :custom-face
  (company-tooltip
   ((t (:background "black" :foreground "white"))))
  (company-tooltip-selection
   ((t (:background "DodgerBlue" :foreground "yellow3" :weight bold))))
  (company-tooltip-common ((t (:weight bold :foreground "DodgerBlue"))))
  (company-tooltip-common-selection ((t (:weight bold :foreground "white"))))
  (company-scrollbar-fg ((t (:background "ivory4"))))
  (company-scrollbar-bg ((t (:background "ivory3"))))
  (company-tooltip-annotation ((t (:foreground "DarkCyan")))))

;; Configure projectile
(use-package projectile
  :config (projectile-mode 1)
  :bind (("C-?" . projectile-find-other-file)))

;; Configure helm
(use-package helm
  :init
  (helm-mode 1)
  :config
  (require 'helm-config)
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

  (define-key 'help-command (kbd "C-f") 'helm-apropos)
  (define-key 'help-command (kbd "r") 'helm-info-emacs)
  (define-key 'help-command (kbd "C-l") 'helm-locate-library)
  (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
  (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-c r" . helm-recentf)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-c h o" . helm-occur)
         ("C-c h g" . helm-google-suggest)
         ("C-c h x" . helm-register)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map helm-grep-mode-map
         ("<return>" . helm-grep-mode-jump-other-window)
         ("n" . helm-grep-mode-jump-other-window-forward)
         ("p" . helm-grep-mode-jump-other-window-backward))

  :custom-face
  (helm-source-header ((nil (:foreground "dark magenta" (:weight bold (:background black (:font "Ubuntu-Mono 14")))))))
  (helm-selection ((nil (:foreground "white" (:background "SpringGreen4")))))
  (helm-buffer-modified ((nil (:foreground "RosyBrown")))))

;; Configure helm-projectile
(use-package helm-projectile
  :after helm
  :init
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien))

(use-package helm-wikipedia
  :after helm
  :bind ("C-c h w" . helm-wikipedia-suggest))

;; Configure shell
(use-package shell
  :bind (:map shell-mode-map
              ("<C-right>" . windmove-right)
              ("<C-left>" . windmove-left)
              ("<C-up>" . windmove-up)
              ("<C-down>" . windmove-down)))

(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

;; Dired+ has to be downloaded from EmacsWiki.
(let* ((diredplus-file (expand-file-name "dired+/dired+.el" user-emacs-directory))
      (diredplus-dir (file-name-directory diredplus-file)))
  (if (file-exists-p diredplus-file)
    (message "Dired+ does already exist")
  (unless (file-directory-p diredplus-dir)
    (make-directory diredplus-dir))
  (url-copy-file "https://www.emacswiki.org/emacs/download/dired%2b.el"
                 diredplus-file)))

(use-package flycheck)

;; Configure Dired+
(use-package dired+
  :load-path "dired+"
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

(require 'org-setup)

;;---------------------------------Hooks------------------------------------------------

(add-hook 'after-init-hook (lambda () (server-start)))
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace 1)))

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
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u c") 'uncomment-region)

;; Global Unset
(global-unset-key (kbd "C-x c")) ;; Recommended in Helm tutorial, see http://tuhdo.github.io/helm-intro.html
(global-unset-key (kbd "C-x f"))

(provide 'basic-setup)
