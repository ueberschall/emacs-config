(defun read-path-from-file (file)
  "Read the first path from file"
  (when (file-exists-p file)
    (save-excursion
      (let ((buf (find-file-literally file)))
        (set-buffer buf)
        (setq path (buffer-string))
        (kill-buffer)
        (replace-regexp-in-string "\n$" "" path)))))

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun duplicate-line ()
  "Duplicate the line up to cursor position on next line"
  (interactive)
  (set-mark-command nil)
  (move-beginning-of-line nil)
  (kill-ring-save nil nil t)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1)
  (yank)
  (deactivate-mark))

(defun kill-all-buffers-with-parent-directory ()
  "Kill all buffers which got a specific directory as root"
  (interactive)
  (save-excursion
    (let ((directory (read-directory-name "Directory: " (expand-file-name default-directory) nil t))
          (currentBuffers (buffer-list)))
      (when (string-prefix-p "~/" directory)
        (setq directory (replace-regexp-in-string "^~" (getenv "HOME") directory)))
      (dolist (buffer currentBuffers)
        (when (and (buffer-file-name buffer)
                   (string-match-p (regexp-quote directory)
                                   (buffer-file-name buffer)))
          (kill-buffer buffer))))))

(defun save-and-kill-outdated-buffers (nb-of-buffers-to-keep)
  "Keep only the most nb-of-buffers-to-keep recent file buffers and kill the rest"
  (save-some-buffers t)
  (setq i 0)
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (setq i (+ i 1)))
    (unless (or (eq buf (current-buffer))
                (and (buffer-file-name buf)
                     (<= i nb-of-buffers-to-keep))
                (and (string-prefix-p "*" (buffer-name buf))
                     (string-suffix-p "*" (buffer-name buf))))
      (kill-buffer buf))))

(defun compile-with-prefix-arg ()
  "Call compile command but with prefix arg."
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'compile))

(defun replace-string-in-directory (directory from-string to-string)
  "Replace FROM-STRING with TO-STRING in all files in DIRECTORY."
  (interactive "DDirectory: \nsFrom string: \nsTo string: ")
  (let ((files (directory-files-recursively directory ".*")))
    (dolist (file files)
      (find-file file)
      (goto-char (point-min))
      (while (search-forward from-string nil t)
        (replace-match to-string nil t))
      (save-buffer)
      (kill-buffer))))


(provide 'basic-functions)
