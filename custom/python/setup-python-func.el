(defun load-pyvenv-path-from-file (file)
  "Activate the pyvenv which path is specified in file"
  (when (file-exists-p file)
    (save-excursion
      (let ((buf (find-file-literally file)))
        (set-buffer buf)
        (setq path (buffer-string))
        (kill-buffer)
        (replace-regexp-in-string "\n$" "" path)))))

(defun query-and-store-pyvenv-path (file)
  "Query from user the desired pyvenv path"
  (when (y-or-n-p "Do you want to load a Pyvenv? ")
    (setq pyvenv-dir-p nil)
    (while (not pyvenv-dir-p)
      (setq pyvenv-dir (read-directory-name "Pyvenv directory path: " (load-pyvenv-path-from-file file) nil t))
      (setq pyvenv-dir-p (file-exists-p
                          (expand-file-name "bin/activate" pyvenv-dir)))
      (when (not pyvenv-dir-p)
        (unless (y-or-n-p (concat pyvenv-dir " is not a Pyvenv directory! Select another directory? "))
          (setq pyvenv-dir-p t))))
    (let ((buf (find-file-literally file)))
      (set-buffer buf)
      (erase-buffer)
      (insert pyvenv-dir)
      (save-buffer)
      (kill-buffer))))

(provide 'setup-python-func)
