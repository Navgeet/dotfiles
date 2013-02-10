;; Much of the functionality here is already in emms-mode-line.el
;; I noticed it after I finished writing this.
;; Though this works for multiple buffers. Switch to appropriate buffer
;; and use (nav-emms-mode-line 1)
;; Also it uses a timer to show the time elapsed.

;;; Variables (mainly for caching)

(defvar nav/emms-mode-line-buffer-list nil
  "Buffers in which to update mode-line")

(defvar nav/emms-mode-line-original-format nil
  "The original mode-line-format.")
(make-variable-buffer-local 'nav/emms-mode-line-original-format)

(defvar nav/emms-mode-line-player-status "."
  "'.' - player is stopped
'>' - player is running
'|' - player is paused
Used in the mode-line.")

(defvar nav/emms-mode-line-total-time "0:00"
  "Total time for the current track.")

(defvar nav/emms-mode-line-current-track ""
  "The current track in the format:
Artist - Album - Track.")

(defvar nav/emms-mode-line-year "[]"
  "Year for the current track.")

(defvar nav/emms-mode-line-time-elapsed "0:00"
  "Time elapsed for the current track.")

(defvar nav/emms-mode-line-time-elapsed-num 0
  "Time elapsed (in seconds) for the current track.")

;;; Functions to update the variables

(defun nav/emms-mode-line-update-player-status ()
  (setq nav/emms-mode-line-player-status
	(cond
	 (emms-player-paused-p (progn (nav/emms-mode-line-cancel-timer)
				      "|"))
	 (emms-player-playing-p (progn (nav/emms-mode-line-set-timer)
				       ">"))
	 (t ".")))
  (dolist (buffer nav/emms-mode-line-buffer-list)
    (with-current-buffer buffer
      (force-mode-line-update))))

(defun nav/emms-mode-line-update-vars-on-player-started ()
  "Update the various vars in emms-mode-line when a new track is played.
Hooked to emms-player-started-hook."
  (interactive)
  (with-current-buffer emms-lyrics-buffer
    (let* ((track (emms-playlist-current-selected-track))
	   (artist (emms-track-get track 'info-artist))
	   (album (emms-track-get track 'info-album))
	   (title (emms-track-get track 'info-title))
	   (year (emms-track-get track 'info-year))
	   (total-time (or (emms-track-get track 'info-playing-time) 0)))
      (setq nav/emms-mode-line-time-elapsed-num 0)
      (nav/emms-mode-line-set-timer)
      (setq nav/emms-mode-line-player-status ">")
      (setq nav/emms-mode-line-year (format "[%s]" year))
      (setq nav/emms-mode-line-current-track
	    (format "%s - %s - %s" artist album title))
      (setq nav/emms-mode-line-total-time (format-seconds "%m:%s" total-time))
      (dolist (buffer nav/emms-mode-line-buffer-list)
	(with-current-buffer buffer
	  (force-mode-line-update))))))

(defun nav/emms-mode-line-update-every-second ()
  (setq nav/emms-mode-line-time-elapsed-num
	(1+ nav/emms-mode-line-time-elapsed-num))
  (setq nav/emms-mode-line-time-elapsed
	(format-seconds "%m:%s" nav/emms-mode-line-time-elapsed-num))
  (dolist (buffer nav/emms-mode-line-buffer-list)
    (with-current-buffer buffer
      (force-mode-line-update))))

(defun nav/emms-mode-line-set-timer ()
  (setq nav/emms-mode-line-timer
	(run-at-time "1 sec" 1 'nav/emms-mode-line-update-every-second)))

(defun nav/emms-mode-line-cancel-timer ()
  (cancel-timer nav/emms-mode-line-timer))

;;; Hooks

(add-hook 'emms-player-started-hook
	  'nav/emms-mode-line-update-vars-on-player-started)
(add-hook 'emms-player-finished-hook 'nav/emms-mode-line-update-player-status)
(add-hook 'emms-player-finished-hook 'nav/emms-mode-line-cancel-timer)
(add-hook 'emms-player-stopped-hook 'nav/emms-mode-line-update-player-status)
(add-hook 'emms-player-stopped-hook 'nav/emms-mode-line-cancel-timer)
(add-hook 'emms-player-paused-hook 'nav/emms-mode-line-update-player-status)

;;; User Interface

(defmacro nav/emms-mode-line-format ()
  "Expands into the mode-line-format to use."
  `(list " " 'nav/emms-mode-line-player-status " "
	 "[" 'nav/emms-mode-line-time-elapsed " / " 'nav/emms-mode-line-total-time "] "
	 '(:propertize nav/emms-mode-line-current-track face mode-line-emphasis)
	 " "
	 'nav/emms-mode-line-year))

(defun nav/emms-mode-line (arg)
  "Turn on `emms-mode-line' if ARG is positive, off otherwise."
  (interactive "p")
  (if (and arg (> arg 0))
      (progn
	(setq nav/emms-mode-line-buffer-list (cons (current-buffer) nav/emms-mode-line-buffer-list))
	(setq nav/emms-mode-line-original-format mode-line-format)
	(setq mode-line-format (nav/emms-mode-line-format))
	(force-mode-line-update))
    (dolist (buffer nav/emms-mode-line-buffer-list)
      (with-current-buffer buffer
	(setq mode-line-format nav/emms-mode-line-original-format)
	(force-mode-line-update)))
    (nav/emms-mode-line-cancel-timer)))

(provide 'nav-emms-mode-line)
;; nav-emms-mode-line.el ends here
