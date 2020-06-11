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

(defun insert-doxygen-documentation-for-variable (variableName)
  "Inserts doxygen documentation for variableName"
  (save-excursion
    (back-to-indentation)
    (insert-and-indent "//==============================================================================")
    (insert-and-indent (format "//! \\brief Short description of variable %s" variableName))
    (insert-and-indent "//------------------------------------------------------------------------------")))

(defun insert-doxygen-documentation-for-class (className)
  "Inserts doxygen documentation for className"
  (save-excursion
    (back-to-indentation)
    (insert-and-indent "//==============================================================================")
    (insert-and-indent (format "//! \\brief Short description of class %s" className))
    (insert-and-indent "//------------------------------------------------------------------------------")))

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
    (insert-and-indent "//==============================================================================")
    (insert-and-indent (format "//! \\brief Short description of function %s" functionName))
    (insert-and-indent "//!"))
  (insert-doxygen-documentation-for-parameter-list)
  (save-excursion
    (back-to-indentation)
    (insert-and-indent "//!")
    (insert-and-indent "//! \\returns void")
    (insert-and-indent "//------------------------------------------------------------------------------")))

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

(defun kill-all-buffers-with-parent-directory ()
  "Kill all buffers which got a specific directory as root"
  (interactive)
  (save-excursion
    (let ((directory (read-directory-name "Directory: " default-directory nil t))
          (currentBuffers (buffer-list)))
      (dolist (buffer currentBuffers)
        (when (and (buffer-file-name buffer)
                   (string-match-p (regexp-quote directory)
                                   (buffer-file-name buffer)))
          (kill-buffer buffer))))))


(provide 'setup-cpp-func)
