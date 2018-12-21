(require 'semantic)

(global-semanticdb-minor-mode nil)
(global-semantic-idle-scheduler-mode nil)
(global-semantic-stickyfunc-mode 1)

(semantic-mode nil)

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

;; Enable EDE only in C/C++
;; (require 'ede)
;; (global-ede-mode)

(provide 'setup-cedet)
