;; supplies nav/emms-reco-api-key
(require 'nav-emms-reco-api-key)

(defvar nav/emms-reco-artist-face 'rainbow-delimiters-depth-5-face)
(defvar nav/emms-reco-track-face 'widget-documentation)
(defvar nav/emms-reco-similiar-tracks-buffer-name " *EMMS Similiar Tracks*")
(defvar nav/emms-reco-similiar-tracks-file nil)

(defun nav/url-escape-point (c)
  "Escape (quote) a character for a URL"
  (format "%%%X" (string-to-char c)))

(defun nav/url-quote-str-utf8 (s)
  "Quote special characters in a URL string"
  (let ((unquoted-re "[^a-zA-Z0-9_./-?&]")
	(encoded (encode-coding-string s 'utf-8))
	(n 5))
    (while (setq n (string-match unquoted-re encoded n))
      (setq encoded
	    (replace-match (nav/url-escape-point (match-string 0 encoded))
			   t t encoded)
	    n (1+ n)))
    encoded))

(defun nav/emms-reco-similiar-tracks-write-to-buffer (parsed-track-list)
  (with-current-buffer nav/emms-reco-similiar-tracks-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (mapc (lambda (x)
	      (let ((artist (caddr (assoc 'name (assoc 'artist x))))
		    (track (caddr (assoc 'name x))))
		(insert (propertize artist
				    'face nav/emms-reco-artist-face
				    'artist artist
				    'track track))
		(insert (propertize " - "
				    'artist artist
				    'track track))
		(insert (propertize (concat track "\n")
				    'face nav/emms-reco-track-face
				    'artist artist
				    'track track))))
	    parsed-track-list)
      (goto-char (point-min)))))

(defun nav/emms-reco-similiar-tracks-read-file (file)
  (let* ((filename (replace-regexp-in-string
		    (concat "\\." (file-name-extension file) "\\'")
		    ".tracks"
		    (file-name-nondirectory file)))
	 (dir (concat (file-name-directory file)
		      ".similiar_tracks/"))
	 (recofile (concat dir filename)))
  (if (file-exists-p recofile)
      (with-current-buffer nav/emms-reco-similiar-tracks-buffer
	(let ((inhibit-read-only t))
	  (insert-file-contents recofile)))
    (nav/emms-reco-similiar-tracks-write-file file recofile))))

(defun nav/emms-reco-similiar-tracks-retrieve-url (&rest _)
  (with-current-buffer nav/emms-reco-temp-buffer
    (forward-line 16)
    (let ((parsed (libxml-parse-xml-region (point) (point-max))))
      (nav/emms-reco-similiar-tracks-write-to-buffer (nthcdr 2 (caddr parsed)))))
  (with-current-buffer nav/emms-reco-similiar-tracks-buffer
    (write-region nil nil nav/emms-reco-similiar-tracks-file)))

(defun nav/emms-reco-similiar-tracks-write-file (origfile recofile)
  (let* ((track (gethash origfile emms-cache-db))
	 (artist (emms-track-get track 'info-artist))
	 (title (emms-track-get track 'info-title))
	 (url-request-extra-headers
	  '(("User-Agent" . "Wget/1.14 (linux-gnu)")))
	 (url (nav/url-quote-str-utf8 (format
				       "http://ws.audioscrobbler.com/2.0/?method=track.getSimilar&api_key=%s&track=%s&artist=%s&autocorrect=1"
				  nav/emms-reco-api-key
				  title
				  artist)))
	 (dir (file-name-directory recofile)))
    (unless (file-exists-p dir)
      (make-directory dir))
    (setq nav/emms-reco-similiar-tracks-file recofile)
    (setq nav/emms-reco-temp-buffer (url-retrieve
				     url
				     (lambda (&rest _)
				       (nav/emms-reco-similiar-tracks-retrieve-url _))))))

(defun nav/emms-reco-similiar-tracks-create-buffer ()
  (setq nav/emms-reco-similiar-tracks-buffer (get-buffer-create nav/emms-reco-similiar-tracks-buffer-name))
  (set-buffer nav/emms-reco-similiar-tracks-buffer)
  (hl-line-mode 1)
  (enriched-mode 1)
  (setq buffer-read-only t))

(defun nav/emms-reco-similiar-tracks-update (track)
  (interactive)
  (let ((file (emms-track-get track 'name)))
    (nav/emms-reco-similiar-tracks-read-file file)))

(provide 'nav-emms-reco)
;;; nav-emms-reco.el ends here
