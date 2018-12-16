(defun concat-two-lists (alist blist)
  "Concatenate two lists"
  (if (and (> (length alist) max-lisp-eval-depth) (> (length blist) max-lisp-eval-depth))
      (message "Input lists are not allowed to be longer than %d elements" max-lisp-eval-depth)
    (progn
      (if (cdr alist)
	  (setq blist (concat-two-lists (cdr alist) blist)))
      (cons (car alist) blist))))

(defun concat-multiple-lists (list-of-lists)
  "Concatenate multiple lists"
  (setq begin (car list-of-lists))
  (setq rest (cdr list-of-lists))
  (while rest
    (setq begin (concat-two-lists begin (car rest)))
    (setq rest (cdr rest)))
  (setq output begin))

(defun install-necessary-packages (necessaryPackages)
  "Install the the packages in 'necessaryPackages', if they are not already there"
  (mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
	necessaryPackages))

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
