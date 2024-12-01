(setq c-identifier-regex "\\_<[[:alpha:]][[:word:]_]*\\(?:::[[:alpha:]][[:word:]_]*\\)*")
(setq c-type-regex "\\_<[[:alpha:]][[:word:]_<>:]*\\(?:&\\{0,2\\}\\|\\*\\)?")

(defun doxygen-documentation-for-variable (variable-name)
  "Return doxygen documentation for variable"
  (list (format "\\brief Variable %s" variable-name)))

(defun doxygen-documentation-for-function-parameters (parameter-list)
  "Return doxygen documentation for function parameters"
  (let (doc-string-list)
    (dolist (param parameter-list doc-string-list)
    (if (string-match "const[[:space:]]+" param)
        (push (format "\\param[in]  %s" (substring param (match-end 0) nil)) doc-string-list)
      (push (format "\\param[out] %s" param) doc-string-list)))
    (nreverse doc-string-list)))

(defun doxygen-documentation-for-function (function-name parameter-list return-value)
  "Return doxygen documentation for function, its parameter list and return value"
  (let ((doc-header (list (format "\\brief Function %s" function-name)))
        (doc-parameters (doxygen-documentation-for-function-parameters parameter-list))
        (doc-footer (list (format "\\returns "))))
    (if return-value
        (append doc-header doc-parameters doc-footer)
      (append doc-header doc-parameters))))

(defun doxygen-documentation-for-class (class-name)
  "Return doxygen documentation for class"
  (list (format "\\brief Class %s" class-name)))

(defun doxygen-documentation-for-struct (struct-name)
  "Return doxygen documentation for struct"
  (list (format "\\brief Struct %s" class-name)))

(defun doxygen-documentation-for-alias (alias-name)
  "Return doxygen documentation for alias"
  (list (format "\\brief Alias %s" alias-name)))

(defun doxygen-documentation-for-template-parameters (tparam-list)
  "Return doxygen documentation for a list of template parameters"
  (let (doc-string-list)
    (dolist (tparam tparam-list doc-string-list)
      (push (format "\\tparam      %s" tparam) doc-string-list))
    (nreverse doc-string-list)))

(defun doxygen-documentation-for-template (template-name template-parameters function-parameters returns-p)
  "Return doxygen documentation for template class or function"
  (let ((doc-header (list (format "\\brief Template %s" template-name)))
        (doc-tparams (doxygen-documentation-for-template-parameters template-parameters))
        (doc-fparams (doxygen-documentation-for-function-parameters function-parameters))
        (doc-footer (list "\\returns ")))
    (if returns-p
        (append doc-header doc-tparams doc-fparams doc-footer)
      (append doc-header doc-tparams doc-fparams))))

;;------------------------------------------------------------------------------
;; Function for extracting information necessary for Doxygen documentation
;; generation from buffer or string

(setq comment-begin-regex "\\(?://\\)\\|\\(?:/\\*\\)")
(setq template-regex "\\_<template<.*>")
(setq class-regex "\\_<class\\_>")
(setq struct-regex "\\_<struct\\_>")
(setq alias-regex "\\_<using\\_>")

(defun extract-parameters-from-string (parameter-string)
  "Extract function parameters into list.
If they have a 'const' as prefix word, it is forwarded into the list.
The type however is not forwarded."
  (let ((parameters ()))
    (while (string-match
            (format "\\(const[[:space:]]+\\)?[[:space:]]*\\(%s\\)\\.\\{0,3\\}[[:space:]]+\\(%s\\)[[:space:]]*[=,)]"
                    c-type-regex c-identifier-regex)
            parameter-string)
      (if (match-end 1)
          (push (concat "const " (match-string 3 parameter-string)) parameters)
        (push (match-string 3 parameter-string) parameters))
      (setq parameter-string (substring parameter-string (match-end 0))))
    (nreverse parameters)))

(defun extract-template-parameters-from-string (template-parameter-string)
  "Extract template parameters from string into list"
  (let (template-parameters)
    (while (string-match
           (format "[[:space:]\n]*%s\\.\\{0,3\\}[[:space:]\n]+\\(%s\\)[[:space:]\n]*[=,>]"
                   c-type-regex c-identifier-regex)
           template-parameter-string)
      (push (match-string 1 template-parameter-string) template-parameters)
      (setq template-parameter-string (substring template-parameter-string (match-end 0))))
    (nreverse template-parameters)))

(defun extract-doxygen-documentation-for-variable-after-point ()
  "Reads variable declaration after point and returns its doxygen documentation"
  (when (looking-at c-identifier-regex)
    (doxygen-documentation-for-variable (match-string-no-properties 0))))

(defun extract-doxygen-documentation-for-class-after-point ()
  "Reads class declaration after point and returns its doxygen documentation"
  (when (looking-at (format "%s[[:space:]\n]+\\(%s\\)" class-regex c-identifier-regex))
    (doxygen-documentation-for-class (match-string-no-properties 1))))

(defun extract-doxygen-documentation-for-struct-after-point ()
  "Reads struct declaration after point and returns its doxygen documentation"
  (when (looking-at (format "%s[[:space:]\n]+\\(%s\\)" struct-regex c-identifier-regex))
    (doxygen-documentation-for-class (match-string-no-properties 1))))

(defun extract-doxygen-documentation-for-alias-after-point ()
  "Reads alias declaration after point and returns its doxygen documentation"
  (when (looking-at (format "%s[[:space:]\n]+\\(%s\\)[[:space:]]*=" alias-regex c-identifier-regex))
    (doxygen-documentation-for-alias (match-string-no-properties 1))))

(defun extract-doxygen-documentation-for-function-after-point ()
  "Reads function declaration after point and returns its doxygen documentation"
  (when (looking-at (format "\\(%s\\)(\\(\\(?:.*?\n?\\)+)\\)" c-identifier-regex))
    (doxygen-documentation-for-function (match-string-no-properties 1)
                                        (extract-parameters-from-string (match-string-no-properties 2))
                                        (and (looking-back (format "%s[[:space:]]+" c-type-regex))
                                             (not (looking-back "void[[:space:]]+"))))))

(defun extract-doxygen-documentation-for-template-after-point ()
  "Reads template declaration after point and returns its doxygen documentation"
  (if (looking-at
       (format
        "template<\\(.*>\\)[[:space:]\n]+\\(class\\|struct\\|using\\)[[:space:]\n]+\\(%s\\)"
        c-identifier-regex))
      (let ((ttype (match-string-no-properties 2))
            (ttype-name (match-string-no-properties 3))
            (tparam-string (match-string-no-properties 1)))
        (doxygen-documentation-for-template
         (concat ttype " " ttype-name)
         (extract-template-parameters-from-string tparam-string)
         nil nil))
    (when
        (looking-at
         (format
          "template<\\(.*>\\)[[:space:]\n]+\\(%s[[:space:]\n]\\)+?[[:space:]\n]*\\(%s\\)(\\(\\(?:.*?\n?\\)+)\\)"
          c-type-regex c-identifier-regex))
      (let ((tfunc-name (match-string-no-properties 3))
            (tparam-string (match-string-no-properties 1))
            (fparam-string (match-string-no-properties 4))
            (return-p (not (string-match-p "\\<void\\>" (match-string-no-properties 2)))))
        (doxygen-documentation-for-template
         (concat "function " tfunc-name)
         (extract-template-parameters-from-string tparam-string)
         (extract-parameters-from-string fparam-string)
         return-p)))))

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
