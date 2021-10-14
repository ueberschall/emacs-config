(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (file-exists-p (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))

(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))

(defun generate-getters-cpp ()
  "Generate setter functions for all member variables in region"
  (interactive)
  (save-excursion
    (save-restriction
      (if (use-region-p)
          (progn
            (narrow-to-region (region-beginning) (region-end))
            (goto-char 1)
            (while (search-forward-regexp "\\([a-zA-Z1-9_:<>]+\\)\\Wm_\\(\\w\\)\\([a-zA-Z1-9_]+\\)\\(?:{.*}\\)?;"
                                          nil t)
              (replace-match (concat (match-string 1) " get"
                                     (upcase (match-string 2))
                                     (match-string 3) "()"
                                     " const { return m_" (match-string 2) (match-string 3) "; }"))))))))

(defun generate-setters-cpp ()
  "Generate setter functions for all member variables in region"
  (interactive)
  (save-excursion
    (save-restriction
      (if (use-region-p)
          (progn
            (narrow-to-region (region-beginning) (region-end))
            (goto-char 1)
            (while (search-forward-regexp "\\([a-zA-Z1-9_:<>]+\\)\\Wm_\\(\\w\\)\\([a-zA-Z1-9_]+\\)\\(?:{.*}\\)?;"
                                          nil t)
              (replace-match (concat "void set"
                                     (upcase (match-string 2))
                                     (match-string 3)
                                     "(" (match-string 1) " " (match-string 2) (match-string 3) ") "
                                     "{ m_" (match-string 2) (match-string 3) " = "
                                     (match-string 2) (match-string 3) "; }"))))))))

(defun is-cpp-file (file)
  "Checks whether file is a cpp file"
  (setq extension (file-name-extension file))
  (or (equal extension "hpp")
      (equal extension "cpp")))

(defun replace-include-directives-in-file (file old-include new-include)
  "Replaces all occurences of old-include with new-include in file"
  (when (is-cpp-file file)
    (let ((work-buffer (find-file-noselect file))
          (search-regexp (concat "\\(# *include +<\\|\"\\)" old-include))
          (replace-string (concat "\\1" new-include)))
      (save-excursion
        (set-buffer work-buffer)
        (goto-char (point-min))
        (while (re-search-forward search-regexp nil t)
          (replace-match replace-string))
        (save-buffer)
        (kill-buffer)))))

(defun replace-include-directives-in-directory (directory old-include new-include)
  "Replaces all occurences of old-include with new-include in directory"
  (dolist (element (directory-files directory t "^[^.].*"))
    (if (eq (file-attribute-type (file-attributes element)) t)
        (replace-include-directives-in-directory element old-include new-include)
      (replace-include-directives-in-file element old-include new-include))))

(defun replace-include-directive ()
  "Replaces an include directive with a new one in a specified directory"
  (interactive)
  (let ((directory (read-directory-name "Directory: " default-directory nil t))
        (old-include (read-string "Include directive to replace: " ))
        (new-include (read-string "Include directive to insert: ")))
    (replace-include-directives-in-directory directory old-include new-include)))

(defun insert-and-indent (args)
  "Insert args in buffer, adds new line and indents after that"
  (insert args)
  (newline-and-indent))

(defun insert-double-line-comment-seperator ()
  "Inserts a 80 characters long horizontal comment seperator consisting of '='"
  (interactive)
  (insert-and-indent "//=============================================================================="))

(defun insert-single-line-comment-seperator ()
  "Inserts a 80 characters long horizontal comment seperator consisting of '-'"
  (interactive)
  (insert-and-indent "//------------------------------------------------------------------------------"))

;; Doxygen documentation generations functions

(setq c-identifier-regex "\\_<[[:alpha:]][[:word:]_]*")
(setq c-type-regex "\\_<[[:alpha:]][[:word:]_<>*&:]*")

(defun doxygen-documentation-for-variable (variable-name)
  "Return doxygen documentation for variable"
  (format "\\brief Variable %s" variable-name))

(defun extract-function-parameters-from-string (parameter-string)
  "Extract function parameters into list.
If they have a 'const' as prefix word, it is forwarded into the list.
The type however is not forwarded."
  (let ((function-parameters ()))
    (while (string-match
            (format "\\(const[[:space:]]+\\)?[[:space:]]*\\(%s\\)[[:space:]]+\\(%s\\)[,)]"
                    c-type-regex c-identifier-regex)
            parameter-string)
      (if (match-end 1)
          (push (concat "const " (match-string 3 parameter-string)) function-parameters)
        (push (match-string 3 parameter-string) function-parameters))
      (setq parameter-string (substring parameter-string (match-end 0))))
    (nreverse function-parameters)))

(defun doxygen-documentation-for-function (function-name parameter-list return-value)
  "Return doxygen documentation for function, its parameter list and return value"
  (setq doc-string (cons (format "\\brief Function %s" function-name) ()))
  (dolist (param parameter-list doc-string)
    (if (string-match "const[[:space:]]+" param)
        (push (format "\\param[in]  %s" (substring param (match-end 0) nil)) doc-string)
      (push (format "\\param[out]  %s" param) doc-string)))
  (when return-value
    (push "\\returns" doc-string))
  (nreverse doc-string))

(defun doxygen-documentation-for-class (class-name)
  "Inserts doxygen documentation for class"
  (format "\\brief Class %s" class-name))

(defun doxygen-documentation-for-struct (struct-name)
  "Inserts doxygen documentation for struct"
  (format "\\brief Struct %s" class-name))

(defun doxygen-documentation-for-using (using-name)
  "Inserts doxygen documentation for struct"
  (format "\\brief Using %s" using-name))

(defun insert-doxygen-documentation-for-parameter-list ()
  "Insert doxygen documentation for parameter list after point"
  (save-excursion
    (setq start (point))
    (setq end (save-excursion (search-forward ")")))
    (setq nbOfParameters 0)
    (save-restriction
      (narrow-to-region start end)
      (setq isInputParameter nil)
      (while (re-search-forward "\\_<[[:alpha:]][[:word:]_]*" nil t)
        (cond ((equal (match-string-no-properties 0) "const")
               (setq isInputParameter t))
              ((looking-at-p ",\\|)")
               (setq nbOfParameters (1+ nbOfParameters))
               (if isInputParameter
                   (kill-new (format "//! \\param[in]  %s" (match-string-no-properties 0)))
                 (kill-new (format "//! \\param[in, out]  %s" (match-string-no-properties 0))))
               (setq isInputParameter nil)))))
    (rotate-yank-pointer nbOfParameters)
    (setq nbOfLeftRotations nbOfParameters)
    (goto-char start)
    (back-to-indentation)
    (while (> nbOfLeftRotations 0)
      (rotate-yank-pointer -1)
      (yank)
      (newline-and-indent)
      (setq nbOfLeftRotations (1- nbOfLeftRotations)))))

(defun insert-doxygen-documentation-for-function (functionName)
  "Inserts doxygen documentation for functionName"
  (save-excursion
    (back-to-indentation)
    (insert-double-line-comment-seperator)
    (insert-and-indent (format "//! \\brief Short description of function %s" functionName))
    (insert-and-indent "//!"))
  (insert-doxygen-documentation-for-parameter-list)
  (save-excursion
    (back-to-indentation)
    (insert-and-indent "//!")
    (insert-and-indent "//! \\returns void")
    (insert-single-line-comment-seperator)))

(defun doxygen-documentation-for-region (start end)
  "Generate doxygen documentation for every relevant expression in the passed region"
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "//\\|/\\*\\|\\_<[[:alpha:]][[:word:]_]*" nil t)
        (cond ((equal (match-string-no-properties 0) "//")
               (move-end-of-line 1))
              ((equal (match-string-no-properties 0) "/*")
               (search-forward "*/"))
              ((or (equal (match-string-no-properties 0) "class")
                   (equal (match-string-no-properties 0) "struct"))
               (re-search-forward "\\_<[[:alpha:]][[:word:]_]*")
               (insert-doxygen-documentation-for-class (match-string-no-properties 0))
               (re-search-forward "{\\|;")
               (when (equal (match-string-no-properties 0) "{")
                 (setq subStart (point))
                 (backward-char)
                 (forward-list)
                 (doxygen-documentation-for-region subStart (1- (point)))))
              ((looking-at-p "(")
               (insert-doxygen-documentation-for-function (match-string-no-properties 0))
               (forward-list)
               (re-search-forward "{\\|;")
               (when (equal (match-string-no-properties 0) "{")
                 (backward-char)
                 (forward-list)))
              ((looking-at-p "{\\|;")
               (insert-doxygen-documentation-for-variable (match-string-no-properties 0))
               (when (looking-at-p "{")
                 (forward-list))))))))

(defun doxygen-documentation-for-region (start end)
  "Generate doxygen documentation for every relevant expression in the passed region"
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let* ((comment-begin-regex "//\\|/\\*")
            (template-regex "template<.*>[[:space:]]+")
            (class-regex "class[[:space:]]+")
            (struct-regex "struct[[:space:]]+")
            (using-regex "using[[:space:]]+")
            (opening-parenthesis-regex "(")
            (semicolon-regex ";"))
        (while (re-search-forward search-regex nil t))))))



(defun doxygen-documentation-for-file (file)
  "Generate doxygen documentation for file"
    (save-excursion
      (set-buffer (find-file-noselect file))
      (doxygen-documentation-for-region (point-min) (point-max))))

(defun doxygen-documentation-for-file-interactive ()
  "Interactive version of doxygen-documentation-for-file"
  (interactive)
  (setq file (read-file-name "File: " nil (buffer-file-name) t nil 'is-cpp-file))
  (doxygen-documentation-for-file file))

;; Insert '//!' after inserting newline
(defun newline-and-doxygen-comment ()
  "Start new Doxygen comment after entering newline while being in Doxygen block."
  )

(provide 'setup-cpp-func)
