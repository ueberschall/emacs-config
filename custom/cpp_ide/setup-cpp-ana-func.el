(defun clang-format-when-ibeo-sdk-active ()
  "Format the buffer using clang-format when the variable ibeo-sdk-active variable is set and C/C++ mode is active"
  (interactive)
  (when (and ibeo-sdk-active
             (or (string-equal major-mode "c-mode")
                 (string-equal major-mode "c++-mode")))
    (clang-format-buffer)))

(provide 'setup-cpp-ana-func)
