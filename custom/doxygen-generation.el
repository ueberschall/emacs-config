;; Functions for inserting Doxygen documentation into buffer

(defun insert-and-indent (args)
  "Insert args in buffer, adds new line and indents after that"
  (insert args)
  (newline-and-indent))

(defun insert-double-line-comment-seperator (long-p)
  "Inserts a 80 characters long horizontal comment seperator consisting of '='"
  (interactive "P")
  (if long-p
      (insert-and-indent "//==============================================================================")
    (insert-and-indent "//========================================")))

(defun insert-single-line-comment-seperator (long-p)
  "Inserts a 80 characters long horizontal comment seperator consisting of '-'"
  (interactive "P")
  (if long-p
      (insert-and-indent "//------------------------------------------------------------------------------")
    (insert-and-indent "//--------------------------------------")))

(defun insert-doxygen-block (doc-string-list long-p)
  "Insert doxygen documentation into buffer"
  (when doc-string-list
    (back-to-indentation)
    (insert-double-line-comment-seperator long-p)
    (dolist (doc-line doc-string-list)
      (insert-and-indent (concat "//! " doc-line)))
    (insert-single-line-comment-seperator long-p)))

(defun extract-and-insert-doxygen-documentation-for-symbol-under-point (arg)
  "Extracts doxygen documentation for symbol under point and insert it before"
  (interactive "P")
  (let (bound)
    (save-excursion
      (line-move -2)
      (beginning-of-line)
      (setq bound (point)))
    (save-excursion
      (insert-doxygen-block
       (if (re-search-backward template-regex bound t)
          (extract-doxygen-documentation-for-template-after-point)
        (if (re-search-backward class-regex bound t)
            (extract-doxygen-documentation-for-class-after-point)
          (if (re-search-backward struct-regex bound t)
              (extract-doxygen-documentation-for-struct-after-point)
            (if (re-search-backward alias-regex bound t)
                (extract-doxygen-documentation-for-alias-after-point)
              (re-search-backward "\\_<")
              (if (looking-at-p (concat c-identifier-regex "("))
                  (extract-doxygen-documentation-for-function-after-point)
                (if (looking-at-p (concat c-identifier-regex "[{;]?"))
                    (extract-doxygen-documentation-for-variable-after-point)))))))
       arg))))


;; (defun extract-and-insert-doxygen-documentation-for-region (start end long-p)
;;   "Insert doxygen documentation for every relevant expression in the passed region"
;;   (save-excursion
;;     (save-restriction
;;       (narrow-to-region start end)
;;       (goto-char (point-min))
;;       (while (re-search-forward
;;               c-type-regex nil t)
;;         (setq selected-string (match-string-no-properties 0))
;;         (if (string-match-p "/\\*" selected-string)
;;             (re-search-forward "\\*/" nil)
;;           (if (string-match-p "//" selected-string)
;;               (end-of-line)
;;             (if (string-match-p c-type-regex selected-string)
;;                 (if (string-match-p template-regex selected-string)
;;                     (progn
;;                       (re-search-backward template-regex)
;;                       (insert-doxygen-block (extract-doxygen-documentation-for-template-after-point) long-p))
;;                   (if (string-match-p class-regex selected-string)
;;                       (progn
;;                         (re-search-backward class-regex)
;;                         (insert-doxygen-block (extract-doxygen-documentation-for-class-after-point) long-p))
;;                     (if (string-match-p struct-regex selected-string)
;;                         (progn
;;                           (re-search-backward struct-regex)
;;                           (insert-doxygen-block (extract-doxygen-documentation-for-struct-after-point) long-p))
;;                       (if (string-match-p alias-regex selected-string)
;;                           (progn
;;                             (re-search-backward alias-regex)
;;                             (insert-doxygen-block (extract-doxygen-documentation-for-alias-after-point) long-p))
;;                         (if (looking-at-p "(.*)")
;;                             (progn
;;                               (re-search-backward "\\_<")
;;                               (insert-doxygen-block (extract-doxygen-documentation-for-function-after-point) long-p))
;;                           (when (looking-at-p "\\(?:{.*}\\)\\|;")
;;                             (re-search-backward "\\_<")
;;                             (insert-doxygen-block (extract-doxygen-documentation-for-variable-after-point) long-p)))))))
;;               (when (string-match-p "{" selected-string)
;;                 (let (bound
;;                       (start (point)))
;;                   (save-excursion
;;                     (line-move -1)
;;                     (beginning-of-line)
;;                     (setq bound (point)))
;;                   (if (looking-back (format "%s.+\n?.*" class-regex) bound)
;;                       (progn
;;                         (backward-char)
;;                         (forward-sexp)
;;                         (extract-and-insert-doxygen-documentation-for-region start (point) nil))
;;                     (backward-char)
;;                     (forward-sexp)))))))))))



;; (defun extract-and-insert-doxygen-documentation-for-file (file)
;;   "Generate doxygen documentation for file"
;;     (save-excursion
;;       (set-buffer (find-file-noselect file))
;;       (extract-and-insert-doxygen-documentation-for-region (point-min) (point-max) t)))

;; (defun doxygen-documentation-for-file-interactive ()
;;   "Interactive version of doxygen-documentation-for-file"
;;   (interactive)
;;   (setq file (read-file-name "File: " nil (buffer-file-name) t nil 'is-cpp-file))
;;   (extract-and-insert-doxygen-documentation-for-file file))

;; Insert '//!' after inserting newline when in Doxygen comment block
(defun cpp-newline ()
  "Custom newline function for C++
Start new Doxygen comment after entering newline while being in Doxygen block."
  (interactive)
  (let (doxy-comment-p)
    (save-excursion
      (back-to-indentation)
      (setq doxy-comment-p (looking-at-p "//!")))
    (newline-and-indent)
    (when doxy-comment-p
      (insert "//! "))))

(provide 'doxygen-generation)
