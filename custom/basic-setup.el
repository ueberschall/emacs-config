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
(winner-mode 1)
(global-visual-line-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Make 'yes-or-no' queries easier to confirm.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Settings for tabs and indentations.
(setq-default indent-tabs-mode nil) ;; Indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Load the functions for saving and restoring desktops.
(require 'desktop-saving)

;; Speichern des Desktops (Quelle: https://www.emacswiki.org/emacs/Desktop)
(setq desktop-load-locked-desktop t)

(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

(add-hook 'desktop-after-read-hook 'delete-desktop)

(add-hook 'kill-emacs-hook (lambda ()
                             (save-and-kill-outdated-buffers 20)
                             (session-save)))

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

(use-package treemacs
  :config
  (treemacs)
  (treemacs-toggle-fixed-width))

(provide 'basic-setup)
