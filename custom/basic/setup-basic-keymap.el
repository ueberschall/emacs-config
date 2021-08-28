;; Global Set
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile-with-prefix-arg)))
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "C-c w") 'ace-window)
(global-set-key (kbd "<C-right>") 'windmove-right)
(global-set-key (kbd "<C-left>") 'windmove-left)
(global-set-key (kbd "<C-up>") 'windmove-up)
(global-set-key (kbd "<C-down>") 'windmove-down)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

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

(global-set-key (kbd "<C-tab>") 'company-complete)

(global-set-key (kbd "C-c <return>") 'duplicate-line)

(global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
(global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
(global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
(global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)

;; Global Unset
(global-unset-key (kbd "C-x c")) ;; Recommended in Helm tutorial, see http://tuhdo.github.io/helm-intro.html
(global-unset-key (kbd "C-x f"))

;; Help Commands
(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)

;; Mini Buffer
(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

;; Helm Mode
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

;; Shell Mode
(define-key shell-mode-map (kbd "<C-right>") 'windmove-right)
(define-key shell-mode-map (kbd "<C-left>") 'windmove-left)
(define-key shell-mode-map (kbd "<C-up>") 'windmove-up)
(define-key shell-mode-map (kbd "<C-down>") 'windmove-down)

;; Dired Mode
(define-key dired-mode-map (kbd "<C-right>") 'windmove-right)
(define-key dired-mode-map (kbd "<C-left>") 'windmove-left)
(define-key dired-mode-map (kbd "<C-up>") 'windmove-up)
(define-key dired-mode-map (kbd "<C-down>") 'windmove-down)

;; Org Mode
(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-c c") 'org-capture)
(define-key org-mode-map (kbd "<M-s-up>") 'org-metaup-to-beginning)
(define-key org-mode-map (kbd "<M-s-down>") 'org-metadown-to-bottom)

(provide 'setup-basic-keymap)
