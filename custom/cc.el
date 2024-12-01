(require 'cc-functions)

;; Load the functions for the doxygen generation.
(require 'doxygen-generation)

;;--------------------------------------Fix GDB Many windows setup -----------
;; Apparently, GDB does not use always the same window for the source code inspection.
;; Use this piece of code to prevent switching the source code window, when it is
;; already found in another frame
;; (defun my-set-source-window (wrapped true-file line)
;;   "Always use the same window to show source code."
;;   (let ((buf (get-file-buffer true-file)))
;;     (when (and buf gdb-source-window)
;;       (set-window-buffer gdb-source-window buf)))
;;   (let (split-width-threshold split-width-threshold)
;;     (apply wrapped (list true-file line))))

;; (advice-add 'gud-display-line :around #'my-set-source-window)

;;--------------------------------------Configure packages -------------------

;; Configure C/C++-Mode
(use-package cc-mode
  :mode
  (("\\.[ch]$" . c-mode)
   ("\\.[chit]pp$" . c++-mode))
  :init
  (c-add-style "ana_style"
               '("bsd"
                 (c-basic-offset . 4)
                 (c-offset-alists
                  (case-label . *))))
  (add-hook 'c-mode-common-hook (lambda ()
                                  (c-set-style "ana_style")))
  (add-hook 'c-mode-common-hook (lambda ()
                                  (setq-local company-backends '(company-rtags company-capf company-dabbrev company-files company-yasnippet))))
  (add-hook 'c++-mode-hook (lambda ()
                             (c-set-offset 'innamespace 0)))
  :config (setq gc-cons-threshold 100000000
                gdb-many-windows t
                gdb-show-main t
                gud-tooltip-mode 1
                company-global-modes '(not gud-mode))
  :bind (:map c-mode-base-map
              ("RET" . cpp-newline)
              ("C-c =" . insert-double-line-comment-seperator)
              ("C-c -" . insert-single-line-comment-seperator)
              ("C-c d c" . extract-and-insert-doxygen-documentation-for-symbol-under-point)))

(use-package clang-format
  :preface
  (defun clang-format-buffer-smart ()
    "Reformat buffer if .clang-format exists in the projectile root."
    (when (file-exists-p (expand-file-name ".clang-format" (projectile-project-root)))
      (clang-format-buffer)))
  (defun clang-format-buffer-smart-on-save ()
    "Add auto-save hook for clang-format-buffer-smart."
    (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))
  :hook (c-mode-common . clang-format-buffer-smart-on-save)
  :bind (:map c-mode-base-map
              ("C-c f r" . clang-format-region)
              ("C-c f b" . clang-format-buffer)))

(use-package rtags
  :if (executable-find "rdm")
  :pin manual
  :hook (c++-mode . rtags-start-process-unless-running)
  :config
  (setq rtags-path "/usr/local/bin"
        rtags-use-helm t)
  ;; Shutdown rdm when leaving emacs.
  (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
  :bind (:map c-mode-base-map
              ("M-." . rtags-find-symbol-at-point)
              ("M-," . rtags-find-references-at-point)
              ("C->" . rtags-location-stack-forward)
              ("C-<" . rtags-location-stack-back)
              ("M-?" . rtags-find-file)
              ("M-;" . rtags-find-file)
              ("C-." . rtags-find-symbol)
              ("C-," . rtags-find-references)
              ("C-;" . rtags-find-virtuals-at-point)
              ("C-c r r" . rtags-rename-symbol)))

(use-package helm-rtags
  :pin manual
  :after (helm rtags)
  :config
  (setq rtags-display-result-backend 'helm))

(use-package company-rtags
  :pin manual
  :after (company rtags)
  :config
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t))

;;(use-package yasnippet-snippets)

;; (use-package yasnippet
;;   :after yasnippet-snippets
;;   :hook (c++-mode . yas-minor-mode)
;;   :config
;;   (yas-reload-all))

(use-package flycheck-rtags
  :pin manual
  :after (flycheck rtags)
  :init
  ;; ensure that we use only rtags checking
  ;; https://github.com/Andersbakken/rtags#optional-1
  (add-hook 'c-mode-common-hook (lambda ()
                                  (flycheck-mode 1)
                                  (flycheck-select-checker 'rtags)
                                        ;(setq flycheck-global-modes '(cc-mode))
                                  ;; RTags creates more accurate overlays.
                                  (setq-local flycheck-highlighting-mode nil) 
                                  (setq-local flycheck-check-syntax-automatically nil)
                                  ;; Run flycheck 2 seconds after being idle.
                                  (rtags-set-periodic-reparse-timeout 2.0))))

(use-package cmake-mode
  :init
  (add-hook 'cmake-mode-hook
            (lambda ()
              (setq-local company-backends '(company-cmake company-capf company-dabbrev company-files))))
  :config
  (setq-default cmake-tab-width 4))

(provide 'cc-setup)
