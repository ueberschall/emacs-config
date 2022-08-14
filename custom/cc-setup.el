;; ----------------------------------Functions-------------------------
;; Functions for using clang-format
(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (file-exists-p (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))

(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))


;; Functions for generating getter and setter functions
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


;; Functions for replacing include directives in files and directories
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

;; Load the functions for the doxygen generation.
(require 'doxygen-generation)

;; Configure C/C++-Mode
(use-package cc-mode
  :config
  (setq gc-cons-threshold 100000000
        gdb-many-windows t
        gdb-show-main t
        gud-tooltip-mode 1
        company-global-modes '(not gud-mode)))

(use-package cmake-mode
  :config
  (setq-default cmake-tab-width 4))

(provide 'cc-setup)
