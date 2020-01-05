(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (file-exists-p (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))

(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))

(provide 'setup-cpp-func)
