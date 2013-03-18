(defun nav/emms-cache-save (file &rest items)
  "Save cache data structures to a file."
  (message "Saving emms cache...")
  (set-buffer (get-buffer-create " emms-cache "))
  (erase-buffer)
  (insert
   (concat ";;; .emms-cache -*- mode: emacs-lisp; coding: "
	   (symbol-name emms-cache-file-coding-system)
	   "; -*-\n"))
  (mapc
   (lambda (x)
     (cond
      ((listp (symbol-value x)) (insert (format
					 "(setq %s '%S)\n"
					 (symbol-name x) (symbol-value x))))
      ((hash-table-p (symbol-value x)) (insert (format
						"(setq %s %S)\n"
						(symbol-name x) (symbol-value x))))))
   items)
  (when (fboundp 'set-buffer-file-coding-system)
    (set-buffer-file-coding-system emms-cache-file-coding-system))
  (unless (file-directory-p (file-name-directory file))
    (make-directory (file-name-directory file)))
  (write-region (point-min) (point-max) file)
  (kill-buffer (current-buffer))
  (message "Saving emms cache...done"))

(defun nav/emms-cache-restore (file)
  "Restore the names cache from a file."
  (load file t nil t))

(provide 'nav-emms-cache)
;;; nav-emms-cache.el ends here
