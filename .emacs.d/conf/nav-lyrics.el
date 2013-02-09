;; -----------------------------------------------------------------------------
;;; Corrections
(defvar nav/emms-lyrics-buffer-name " *EMMS Lyrics*")

(defun emms-lyrics-create-buffer ()
  "Create `emms-lyrics-buffer' dedicated to lyrics. "
  ;; leading white space in buffer name to hide the buffer
  (setq emms-lyrics-buffer (get-buffer-create nav/emms-lyrics-buffer-name))
  (set-buffer emms-lyrics-buffer)
  (emms-lyrics-viewer-mode)
  
  (setq buffer-read-only nil
        cursor-type t)
  (erase-buffer)
  (mapc (lambda (time-lyric) (insert (cdr time-lyric) "\n"))
        emms-lyrics-alist)
  (goto-char (point-min))
  ;; (hl-line-mode 1)
  ;; (set-face-bold-p 'hl-line t)
  (setq buffer-read-only t))

(defun emms-lyrics-catchup (lrc)
  "Catchup with later downloaded LRC file(full path).
If you write some lyrics crawler, which is running asynchronically,
then this function would be useful to call when the crawler finishes its
job."
  (setq emms-lyrics-start-time (current-time)
	emms-lyrics-pause-time nil
	emms-lyrics-elapsed-time 0)
  (emms-lyrics-read-file lrc t)
  (emms-lyrics-set-timer)
  (emms-lyrics-create-buffer)
  (emms-lyrics-seek (float-time (time-since nav/emms-lyrics-time-track-started))))

(defun emms-lyrics-set-timer ()
  "Set timers for displaying lyrics."
  (setq emms-lyrics-timers '())
  (let ((lyrics-alist emms-lyrics-alist)
        (line 0))
    (while lyrics-alist
      (let ((time (- (caar lyrics-alist) emms-lyrics-elapsed-time))
            (lyric (cdar lyrics-alist))
            (next-time (and (cdr lyrics-alist)
                            (- (car (cadr lyrics-alist))
                               emms-lyrics-elapsed-time)))
            (next-lyric (and (cdr lyrics-alist)
                             (cdr (cadr lyrics-alist)))))
        (setq line (1+ line))
	(when (or (and next-time (> next-time 0))
		  (> time 0))
	  (setq emms-lyrics-timers
		(append emms-lyrics-timers
			(list
			 (run-at-time (format "%d sec" time)
				      nil
				      'emms-lyrics-display-handler
				      lyric
				      next-lyric
				      line
				      (and next-time (- next-time time)))))))
	(setq lyrics-alist (cdr lyrics-alist))))))

(defun emms-lyrics-display (lyric line)
  "Display LYRIC now.
See `emms-lyrics-display-on-modeline' and
`emms-lyrics-display-on-minibuffer' on how to config where to
display."
  (when emms-lyrics-alist
    (when emms-lyrics-display-on-modeline
      (emms-lyrics-mode-line)
      (setq emms-lyrics-mode-line-string lyric)
      ;; (setq emms-lyrics-mode-line-string ; make it fit scroller width
      ;;       (concat emms-lyrics-mode-line-string
      ;;               (make-string
      ;;                (abs (- emms-lyrics-scroll-width (length lyric)))
      ;;                (string-to-char " "))))
      (force-mode-line-update))

    (when emms-lyrics-display-on-minibuffer
      (unless (minibuffer-window-active-p (selected-window))
        (message lyric)))
    
    (when emms-lyrics-display-buffer
      (with-current-buffer emms-lyrics-buffer
	(save-excursion
	  (when line
	    (let ((inhibit-read-only t))
	      (goto-char (point-min))
	      (add-text-properties (point-min) (point-max) '(face default))
	      (forward-line (1- line))
	      (add-text-properties (point) (progn
					     (end-of-line)
					     (point))
				   '(face font-lock-warning-face))))))
      (mapc (lambda (win)
	      (unless (eq (selected-window) win)
		(with-selected-window win
		  (goto-char (point-min))
		  (forward-line (1- line)))))
	    (get-buffer-window-list emms-lyrics-buffer nil t)))))

(defun emms-lyrics-read-file (file &optional catchup)
  "Read a lyric file(LRC format).
Optional CATCHUP is for recognizing `emms-lyrics-catchup'.
FILE should end up with \".lrc\", its content looks like one of the
following:

    [1:39]I love you, Emacs!
    [00:39]I love you, Emacs!
    [00:39.67]I love you, Emacs!

FILE should be under the same directory as the music file, or under
`emms-lyrics-dir'."
  (or catchup
      (setq file (funcall emms-lyrics-find-lyric-function file)))
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (let ((coding-system-for-read emms-lyrics-coding-system))
        (emms-insert-file-contents file)
        (while (search-forward-regexp "^\\[[0-9:.]+\\].*" nil t)
          (let ((lyric-string (match-string 0))
                (time 0)
                (lyric ""))
            (setq lyric
                  (emms-replace-regexp-in-string ".*\\]" "" lyric-string))
            (while (string-match "\\[[0-9:.]+\\]" lyric-string)
              (let* ((time-string (match-string 0 lyric-string))
                     (semi-pos (string-match ":" time-string)))
                (setq time
                      (+ (* (string-to-number
                             (substring time-string 1 semi-pos))
                            60)
                         (string-to-number
                          (substring time-string
                                     (1+ semi-pos)
                                     (1- (length time-string))))))
                (setq lyric-string
                      (substring lyric-string (length time-string)))
                (setq emms-lyrics-alist
                      (append emms-lyrics-alist `((,time . ,lyric))))
                (setq time 0)))))
        (setq emms-lyrics-alist
              (sort emms-lyrics-alist (lambda (a b) (< (car a) (car b))))))
      t)))
;; -----------------------------------------------------------------------------
;;; Scraping

(defvar nav/emms-lyrics-current-lrc-file nil
  "The currrent lrc file with complete path.")

(defvar nav/emms-lyrics-temp-marker nil
  "Temporary marker for the current lrc file.")

(defvar nav/emms-lyrics-marker-db (make-hash-table
			     :test 'equal)
  "Mapping of lyric file paths to their markers.")

(defcustom nav/emms-lyrics-db-cache-file (concat (file-name-as-directory emms-directory)
						 "lyrics_markers_cache")
  "A file used to store lyric markers db."
  :group 'emms
  :type 'file)

(defvar nav/emms-lyrics-time-track-started nil
  "Time when track was started.")

(defun nav/get-lyrics-options (artist song)
  "Return a vector with options for lyrics in order of best match"
  (let* ((str (shell-command-to-string (format "python2 ~/lyricscraper.py \"%s\" \"%s\""
					       artist
					       song)))
	 (str (replace-regexp-in-string "," "" str))
	 (str (replace-regexp-in-string "' '" "\" \"" str))
	 (str (replace-regexp-in-string "\" '" "\" \"" str))
	 (str (replace-regexp-in-string "' \"" "\" \"" str))
	 (str (replace-regexp-in-string "\\['" "[\"" str))
	 (str (replace-regexp-in-string "'\\]" "\"]" str))
	 (vec (read str)))
    vec))

(defun nav/emms-lyrics-start ()
  "Start displaying lryics."
  (let* ((file
	  (emms-track-get
	   nav/emms-currently-playing-track
	   'name))
	 (lrc (emms-replace-regexp-in-string
	       (concat "\\." (file-name-extension file) "\\'")
	       ".lrc"
	       (file-name-nondirectory file))))
    (emms-lyrics-catchup (nav/emms-lyrics-find-lyric lrc))))

(defun nav/emms-lyrics-find-lyric (file)
  "Return full path of found lrc FILE, else scrape it
from the internet. Store it under ./.lyrics/
e.g., (emms-lyrics-find-lyric \"abc.lrc\")"
  (let* ((track nav/emms-currently-playing-track)
	 (dir (concat (file-name-directory (emms-track-get track 'name))
		      ".lyrics/"))
         (lyric-under-curr-dir
          (concat dir file)))
    (if (file-exists-p lyric-under-curr-dir)
	(progn
	  (setq nav/emms-lyrics-current-lrc-file lyric-under-curr-dir)
	  (setq nav/emms-lyrics-temp-marker nil)
	  lyric-under-curr-dir)
      (let* ((artist (emms-track-get track 'info-artist))
	     (song (emms-track-get track 'info-title))
	     (first-url (aref (aref (nav/get-lyrics-options artist song) 0) 2)))
	(unless (file-exists-p dir)
	  (make-directory dir))
	(url-copy-file first-url lyric-under-curr-dir t t)
	(puthash lyric-under-curr-dir 0 nav/emms-lyrics-marker-db)
	(setq nav/emms-lyrics-current-lrc-file lyric-under-curr-dir)
	(setq nav/emms-lyrics-temp-marker nil)
	lyric-under-curr-dir))))

(defun nav/emms-lyrics-fetch-other-option (arg)
  "Get the next lrc file if ARG is positive and update."
  (interactive)
  (let* ((file nav/emms-lyrics-current-lrc-file)
	 (marker (gethash file nav/emms-lyrics-marker-db))
	 (track nav/emms-currently-playing-track)
	 (artist (emms-track-get track 'info-artist))
	 (song (emms-track-get track 'info-title))
	 url)
    (when nav/emms-lyrics-temp-marker
      (setq marker nav/emms-lyrics-temp-marker))
    (cond
     ((> arg 0) (setq marker (1+ marker)))
     ((and (> marker 0) (< arg 0)) (setq marker (1- marker)))
     (t (setq marker 0)))
    (setq nav/emms-lyrics-temp-marker marker)
    (setq url (aref (aref (nav/get-lyrics-options artist song) marker) 2))
    (url-copy-file url "~/.current_temp_lrc" t t)
    (when emms-lyrics-alist
      (mapc #'emms-cancel-timer emms-lyrics-timers))
    (setq emms-lyrics-alist '())
    (setq emms-lyrics-timers '())
    (setq emms-lyrics-elapsed-time (float-time (time-subtract
						nav/emms-lyrics-time-track-started
						(current-time))))
    (emms-lyrics-catchup "~/.current_temp_lrc")))

(defun nav/emms-lyrics-save-current-lrc ()
  (interactive)
  (puthash nav/emms-lyrics-current-lrc-file
	   nav/emms-lyrics-temp-marker
	   nav/emms-lyrics-marker-db)
  (copy-file "~/.current_temp_lrc" nav/emms-lyrics-current-lrc-file t)
  (message "New lyric file in place"))
;; -----------------------------------------------------------------------------
;;; Caching

(defun nav/emms-lyrics-markers-db-save ()
  "Save the lyric marker db to a file."
  (interactive)
  (message "Saving lyric marker db...")
  (set-buffer (get-buffer-create " emms-lyrics-cache "))
  (erase-buffer)
  (insert
   (concat ";;; .emms-lyrics-cache -*- mode: emacs-lisp; coding: "
	   (symbol-name emms-cache-file-coding-system)
	   "; -*-\n"))
  (maphash (lambda (k v)
	     (insert (format
		      "(puthash %S '%S nav/emms-lyrics-marker-db)\n" k v)))
	   nav/emms-lyrics-marker-db)
  (when (fboundp 'set-buffer-file-coding-system)
    (set-buffer-file-coding-system emms-cache-file-coding-system))
  (unless (file-directory-p (file-name-directory nav/emms-lyrics-db-cache-file))
    (make-directory (file-name-directory nav/emms-lyrics-db-cache-file)))
  (write-region (point-min) (point-max) nav/emms-lyrics-db-cache-file)
  (kill-buffer (current-buffer))
  (message "Saving lyric marker db...done"))

(defun nav/emms-lyrics-markers-db-restore ()
  "Restore the names cache from a file."
  (interactive)
  (load nav/emms-lyrics-db-cache-file t nil t))

(add-hook 'kill-emacs-hook 'nav/emms-lyrics-markers-db-save)
(nav/emms-lyrics-markers-db-restore)

;; -----------------------------------------------------------------------------
;;; EMMS Lyrics Viewer Mode

(defcustom emms-lyrics-viewer-mode-hook nil
  "Normal hook run after entering Emms Lyric viewer mode."
  :type 'hook
  :group 'emms-lyrics)

(defvar emms-lyrics-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" (lambda ()
			  (interactive)
			  (later-do 'nav/emms-lyrics-fetch-other-option 1)))
    (define-key map "p" (lambda ()
			  (interactive)
			  (later-do 'nav/emms-lyrics-fetch-other-option -1)))
    (define-key map "s" 'nav/emms-lyrics-save-current-lrc)
    map)
  "Keymap for `emms-lyrics-viewer-mode'.")

(defun emms-lyrics-viewer-mode ()
  "Major mode for viewing lyric files.
\\{emms-lyrics-viewer-mode-map}"
  (interactive)

  (use-local-map emms-lyrics-viewer-mode-map)
  (setq major-mode 'emms-lyrics-viewer-mode
        mode-name "Emms Lyric Viewer")
  (setq buffer-read-only t))

;; -----------------------------------------------------------------------------
;;; Settings

(setq emms-lyrics-find-lyric-function 'nav/emms-lyrics-find-lyric)
(setq emms-lyrics-display-buffer t)
(setq emms-lyrics-display-on-minibuffer t)
(setq emms-lyrics-display-on-modeline nil)
(add-hook 'emms-player-started-hook (lambda ()
				      (interactive)
				      (setq nav/emms-lyrics-time-track-started
					    (current-time))))
(remove-hook 'emms-player-started-hook 'emms-lyrics-start)
(add-hook 'emms-player-started-hook (lambda ()
				      (interactive)
				      (setq emms-lyrics-start-time (current-time)
					    emms-lyrics-pause-time nil
					    emms-lyrics-elapsed-time 0)
				      (later-do 'nav/emms-lyrics-start)))

(provide 'nav-lyrics)
;;; nav-lyrics.el ends here
