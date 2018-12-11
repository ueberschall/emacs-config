;; company-c-headers

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; (use-package cc-mode
;;   :init
;;   (define-key c-mode-map  [(tab)] 'company-complete)
;;   (define-key c++-mode-map  [(tab)] 'company-complete))

(provide 'setup-c)
