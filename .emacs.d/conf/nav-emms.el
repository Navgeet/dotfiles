;; ----
;; EMMS
;; ----

(require 'emms-setup)
(emms-standard)
(emms-default-players)
(setq emms-repeat-playlist nil)

(require 'emms-browser)
(require 'emms-info-metaflac)
(require 'emms-info-mp3info)
(require 'emms-cache)
(require 'emms-lyrics)
(emms-lyrics 1)
(add-to-list 'emms-info-functions 'emms-info-metaflac)
(add-to-list 'emms-info-functions 'emms-info-mp3info)

;;; Correction functions
;; ------------------------------------------------------------------------------
(defvar nav/emms-currently-playing-track nil)

(defun emms-browser-add-tracks ()
  "Add all tracks at point.
Return the previous point-max before adding."
  (interactive)
  (let ((first-new-track (with-current-emms-playlist (point-max)))
        (bdata (emms-browser-bdata-at-point)))
    (emms-browser-playlist-insert-bdata
     bdata (emms-browser-bdata-level bdata))
    (run-hook-with-args 'emms-browser-tracks-added-hook
                        first-new-track)
    (with-current-emms-playlist
      (if (= 1 (point))
	  (emms-playlist-select (point))))
    first-new-track))

(setq emms-info-metaflac-options
  '("--no-utf8-convert"
    "--show-tag=TITLE"
    "--show-tag=ARTIST"
    "--show-tag=ALBUM"
    "--show-tag=DATE"
    "--show-tag=YEAR"
    "--show-tag=TRACKNUMBER"
    "--show-tag=GENRE"))

(defun emms-info-metaflac (track)
  "Get the FLAC tag of file TRACK, using `emms-info-metaflac-program'
and return an emms-info structure representing it."
  (when (and (eq 'file (emms-track-type track))
             (string-match "\\.\\(flac\\|FLAC\\)\\'" (emms-track-name track)))
    (with-temp-buffer
      (when (zerop
             (apply 'call-process
              emms-info-metaflac-program-name
              nil t nil
              "--show-total-samples"
              "--show-sample-rate"
              (append emms-info-metaflac-options
                      (list (emms-track-name track)))))
        (goto-char (point-min))
        (emms-track-set track 'info-playing-time
                        (/ (string-to-number (buffer-substring (point) (line-end-position)))
                           (progn
                             (forward-line 1)
                             (string-to-number (buffer-substring (point) (line-end-position))))))
        (forward-line 1)
	(while (looking-at "^\\([^=\n]+\\)=\\(.*\\)$")
          (let* ((str (cond
		      ((string= (match-string 1) "DATE") "year")
		      (t (downcase (match-string 1)))))
		(name (intern (concat "info-" str)))
                (value (match-string 2)))
            (when (> (length value)
                     0)
              (emms-track-set track
                              name
                              (if (eq name 'info-playing-time)
                                  (string-to-number value)
                                value))))
          (forward-line 1))))))

(defun emms-player-start (track)
  "Start playing TRACK."
  (if emms-player-playing-p
      (error "A player is already playing")
    (let ((player (emms-player-for track)))
      (if (not player)
          (error "Don't know how to play track: %S" track)
        ;; Change default-directory so we don't accidentally block any
        ;; directories the current buffer was visiting.
        (let ((default-directory "/"))
          (funcall (emms-player-get player 'start)
                   track)
	  (setq nav/emms-currently-playing-track track))))))

(let ((func (lambda ()
	      (interactive)
	      (setq nav/emms-currently-playing-track nil))))
  (add-hook 'emms-player-stopped-hook func)
  (add-hook 'emms-player-finished-hook func))

;;; Additional functions
;; ------------------------------------------------------------------------------
(defun nav/emms-names-cache-del (path)
  (let ((name (gethash path nav/emms-path-to-names-db)))
    (remhash name nav/emms-names-cache-db)
    (remhash path nav/emms-path-to-names-db)))

(defun nav/emms-browser-remove-current-node ()
  "Remove the current node, and empty parents.
Also remove all tracks under node from cache."
  (interactive)
  (let ((tracks (emms-browser-tracks-at-point)) path)
    (dolist (track tracks)
      (setq path (emms-track-get track 'name))
      (nav/emms-names-cache-del path)
      (emms-cache-del path)))
  (emms-browser-delete-current-node))

(defun emms-add-dired-and-unmark ()
  "Add all marked directories and unmark all marks"
  (interactive)
  (emms-add-dired)
  (dired-unmark-all-marks))

(defun emms-stop-and-next ()
  (interactive)
  (emms-stop)
  (emms-next)
  (emms-stop))

(define-key emms-browser-mode-map [(delete)] 'nav/emms-browser-remove-current-node)
(define-key dired-mode-map [(insert)] 'emms-add-dired-and-unmark)
(global-set-key (kbd "<f2>") 'emms-volume-lower)
(global-set-key (kbd "<f3>") 'emms-volume-raise)
(global-set-key (kbd "<f5>") 'emms-pause)
(global-set-key (kbd "<f6>") 'emms-stop-and-next)
(global-set-key (kbd "<f7>") 'emms-previous)
(global-set-key (kbd "<f8>") 'emms-next)
;; (add-hook 'emms-player-started-hook 'nav/add-track-to-history-ring)

;;; Interactive selection of songs
;; ------------------------------------------------------------------------------

(defvar nav/emms-names-cache-db (make-hash-table
			     :test 'equal)
  "A mapping of titles to paths.
Used to offer completions for songs in minibuffer.")
 
(defvar nav/emms-path-to-names-db (make-hash-table
			     :test 'equal)
  "A mapping of paths to titles.")
 
(defvar nav/emms-history-ring ())
 
(defun nav/emms-create-new-title (title path)
  (interactive)
  (let* ((list (gethash path emms-cache-db))
	 (artist (cdr (assoc 'info-artist list)))
	 (album (cdr (assoc 'info-album list))))
    (format "(%s)(%s) %s" artist album title)))
 
(defun nav/emms-path-to-names-db-rectify (new-title path)
  (puthash path new-title nav/emms-path-to-names-db))
 
(defun nav/emms-check-title (title path)
  "Check nav/emms-names-cache-db for same TITLE (case insensitive). If found,
prefix (artist)(album) to TITLE and insert. Do this for every match."
  (let ((found-matches-p nil))
    (maphash (lambda (key value)
	       (if (string= (downcase key) (downcase title))
		   (let ((new-title (nav/emms-create-new-title key value)))
		     (progn (puthash new-title
				     value
				     nav/emms-names-cache-db)
			    (remhash key nav/emms-names-cache-db)
			    (setq found-matches-p t)
			    (nav/emms-path-to-names-db-rectify new-title value)))))
	     nav/emms-names-cache-db)
    (if found-matches-p
	(nav/emms-create-new-title title path)
      title)))
 
(defun nav/emms-add-track-to-names-cache-db (track)
  "Update the names db when a track is added to browser."
  (let* ((title (cdr (assoc 'info-title track)))
	 (path (cdr (assoc 'name track)))
	 (full-title (nav/emms-check-title title path)))
    (when (gethash path nav/emms-path-to-names-db)
      (nav/emms-names-cache-del path)
      (setq full-title (nav/emms-check-title title path)))
    (puthash full-title path nav/emms-names-cache-db)
    (puthash path full-title nav/emms-path-to-names-db)))
 
(defun nav/emms-make-names-cache-db ()
  "Utility function to create names-cache-db from emms-cache-db"
  (interactive)
  (maphash (lambda (key value)
	     (let* ((title (cdr (assoc 'info-title value)))
		    (full-title (nav/emms-check-title title key)))
	       (puthash full-title key nav/emms-names-cache-db)
	       (puthash key full-title nav/emms-path-to-names-db)))
	   emms-cache-db))

(defun emms-browser-add-tracks-non-interactive (bdata)
  "Add all tracks at point.
Return the previous point-max before adding."
  (let ((first-new-track (with-current-emms-playlist (point-max))))
    (emms-browser-playlist-insert-bdata
     bdata (emms-browser-bdata-level bdata))
    (run-hook-with-args 'emms-browser-tracks-added-hook
                        first-new-track)
    (with-current-emms-playlist
      (if (= 1 (point))
	  (emms-playlist-select (point))))
    first-new-track))

(defun nav/emms-play-interactively ()
  "Select a track from the minibuffer using `completing-read-ido'."
  (interactive)
  (let* ((song (completing-read-ido "Play: " nav/emms-names-cache-db))
	 (path (gethash song nav/emms-names-cache-db))
	 (track (gethash path emms-cache-db))
	 (name (emms-browser-make-name `(,path ,track) 'info-title))
	 (bdata `((type . info-title)
		  (level . 3)
		  (name . ,name)
		  (data . (,track))))
	 (emms-browser-current-indent ""))
    (emms-browser-add-tracks-non-interactive bdata))
  (unless emms-player-playing-p (emms-start)))

;;; Caching of names-cache-db and history ring
;; ------------------------------------------------------------------------------

(defcustom emms-names-cache-file (concat (file-name-as-directory emms-directory) "names_cache")
  "A file used to store cached file information over sessions."
  :group 'emms
  :type 'file)

(defun nav/emms-dbs-and-history-ring-save ()
  "Save the names cache to a file."
  (interactive)
  (message "Saving emms names cache and history-ring...")
  (set-buffer (get-buffer-create " emms-names-cache "))
  (erase-buffer)
  (insert
   (concat ";;; .emms-names-cache -*- mode: emacs-lisp; coding: "
	   (symbol-name emms-cache-file-coding-system)
	   "; -*-\n"))
  (maphash (lambda (k v)
	     (insert (format
		      "(puthash %S '%S nav/emms-names-cache-db)\n" k v)))
	   nav/emms-names-cache-db)
  (maphash (lambda (k v)
	     (insert (format
		      "(puthash %S '%S nav/emms-path-to-names-db)\n" k v)))
	   nav/emms-path-to-names-db)
  (insert (format
	   "(setq nav/emms-history-ring '%S)\n" nav/emms-history-ring))
  (when (fboundp 'set-buffer-file-coding-system)
    (set-buffer-file-coding-system emms-cache-file-coding-system))
  (unless (file-directory-p (file-name-directory emms-names-cache-file))
    (make-directory (file-name-directory emms-names-cache-file)))
  (write-region (point-min) (point-max) emms-names-cache-file)
  (kill-buffer (current-buffer))
  (message "Saving emms names cache...done"))

(defun nav/emms-dbs-and-history-ring-restore ()
  "Restore the names cache from a file."
  (interactive)
  (load emms-names-cache-file t nil t))

;;; Settings
;; ------------------------------------------------------------------------------
;; My preference for how to display albums/tracks in browser
(setq emms-browser-info-album-format "%i%n")
(setq emms-browser-info-title-format "%i%t")
(setq emms-info-asynchronously nil)
(add-hook 'emms-track-initialize-functions 'nav/emms-add-track-to-names-cache-db t)
(add-hook 'kill-emacs-hook 'nav/emms-dbs-and-history-ring-save)
(nav/emms-dbs-and-history-ring-restore)
(global-set-key (kbd "M-`") 'nav/emms-play-interactively)
(setq emms-browser-alpha-sort-function 'emms-browser-sort-by-year-or-name)

(provide 'nav-emms)
;;; nav-emms.el ends here
