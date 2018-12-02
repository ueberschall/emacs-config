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
