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

(defun insert-and-indent (&rest args)
  "Insert args in buffer, adds new line and indents after that"
  (insert args)
  (newline-and-indent))

(defun insert-doxygen-documentation-for-variable (variableName)
  "Inserts doxygen documentation for variableName"
  (back-to-indentation)
  (insert-and-indent "//==============================================================================")
  (insert-and-indent "//! \\brief Short description of variable " variableName)
  (insert-and-indent "//------------------------------------------------------------------------------"))

(defun insert-doxygen-documentation-for-class (className)
  "Inserts doxygen documentation for className"
  (back-to-indentation)
  (insert-and-indent "//==============================================================================")
  (insert-and-indent "//! \\brief Short description of class " className)
  (insert-and-indent "//------------------------------------------------------------------------------"))

(defun insert-doxygen-documentation-for-parameter-list ()
  "Insert doxygen documentation for parameter list after point"
  (setq end (save-excursion (search-forward ")")))
  (setq isInputParameter nil)
  (while (re-search-forward "\\_<[[:alpha:]][[:word:]_]*" end t)
    (cond ((equal (match-string 0) "const")
           (setq isInputParameter t))
          ((looking-at-p ",\\|)")
           (if isInputParameter
               (insert-and-indent "//! \\param[in] " (match-string 0))
             (insert-and-indent "//! \\param[in, out] " (match-string 0)))
           (setq isInputParameter nil)))))

(defun insert-doxygen-documentation-for-function (functionName)
  "Inserts doxygen documentation for functionName"
  (back-to-indentation)
  (insert-and-indent "//==============================================================================")
  (insert-and-indent "//! \\brief Short description of function " functionName)
  (insert-and-indent "//!")
  (insert-doxygen-documentation-for-parameter-list)
  (insert-and-indent "//!")
  (insert-and-indent "//! \\returns void")
  (insert-and-indent "//------------------------------------------------------------------------------"))

(defun doxygen-documentation-for-region (start end)
  "Generate doxygen documentation for every relevant expression in the passed region"
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "//\\|/\\*\\|\\_<[[:alpha:]][[:word:]_]*" end t)
        (cond ((equal (match-string 0) "//")
               (move-end-of-line 1))
              ((equal (match-string 0) "/*")
               (search-forward "*/" end))
              ((or (equal (match-string 0) "class")
                   (equal (match-string 0) "struct"))
               (re-search-forward "\\_<[[:alpha:]][[:word:]_]*" end t)
               (insert-doxygen-documentation-for-class (match-string 0))
               (re-search-forward "{\\|;" end)
               (when (equal (match-string 0) "{")
                 (setq subStart (point))
                 (backward-char)
                 (forward-list)
                 (doxygen-documentation-for-region subStart (1- (point)))))
              ((looking-at-p "(")
               (insert-doxygen-documentation-for-function (match-string 0))
               (forward-list)
               (re-search-forward "{\\|;" end)
               (when (equal (match-string 0) "{")
                 (backward-char)
                 (forward-list)))
              ((looking-at-p "{\\|;")
               (insert-doxygen-documentation-for-variable (match-string 0))
               (when (looking-at-p "{")
                 (forward-list))))))))


(defun doxygen-documentation-for-file (file)
  "Generate doxygen documentation for file"
  (let ((keep (find-buffer-visiting file))
        (work-buffer (find-file-noselect file)))
    (save-excursion
      (set-buffer work-buffer)
      (doxygen-documentation-for-region (point-min) (point-max))
      (save-buffer)
      (when keep
        (kill-buffer)))))

(defun doxygen-documentation-for-file-interactive ()
  "Interactive version of doxygen-documentation-for-file"
  (setq file (read-file-name "File: " nil nil t nil 'is-cpp-file))
  (doxygen-documentation-for-file file))


(provide 'setup-cpp-func)
