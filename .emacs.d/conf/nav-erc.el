;; ---
;; ERC
;; ---

(require 'erc)
(require 'erc-nicklist)
;; joining && autojoing
;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
;; (erc-autojoin-mode t)
;; (setq erc-autojoin-channels-alist      
;;   '(("localhost" "#mc")))

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
;; don't show any of this
;; (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun my-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "localhost:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "10.109.50.50" :port 411 :nick "BULLStreet"))))

;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e") 'my-erc-start-or-switch) ;; ERC

(defun erc-ignore-unimportant (msg)
  (let ((fags '("apple" "emraan.hashmi" "stolen-my-nick" "windows99")))
    (dolist (fag fags)
      (when (string-match fag msg)
	(setq erc-insert-this nil)))))
(add-hook 'erc-insert-pre-hook 'erc-ignore-unimportant)

(setq  erc-server-coding-system '(windows-1252-unix . windows-1252-dos))
;;       erc-encoding-coding-alist '(("#Mainchat" . cp1252-unix)))

(setq erc-hide-timestamps t)
(setq erc-fill-column 10000)
;; (erc-fill-disable)

(require 'image-dired)
(setq image-dired-display-image-buffer "*my-erc-image-buffer*")
(defun my-display-image-link-in-buffer (url &optional arg)
  (let ((file ".image_link"))
    (url-copy-file url file t t)
    (if (not file)
	(message "Oops couldn't download link")
      (image-dired-create-display-image-buffer)
      (display-buffer image-dired-display-image-buffer)
      (image-dired-display-image file arg)))
  (run-at-time "2 sec" nil (lambda () (delete-windows-on image-dired-display-image-buffer))))

(defvar my-erc-url-buffer "*ERC URLS*")
(defun my-erc-scan-urls ()
  (require 'erc-button)
  
  (save-excursion
    (let ((urls nil)
          (chann (or (erc-default-target)
                     "SERVER")))
      (goto-char (point-min))
      (while (re-search-forward erc-button-url-regexp nil t)
        (push (buffer-substring-no-properties
               (match-beginning 0)
               (match-end 0))
              urls))
      (mapc (lambda (str) (my-display-image-link-in-buffer str)) urls)
      (when urls
        (with-current-buffer (get-buffer-create my-erc-url-buffer)
          (goto-char (point-max))
          (unless (eq (char-before) ?\n)
            (newline))
	  ;; TODO: there should be a hook called for every url
          (insert (mapconcat (lambda (str)
                               (format "%s: %s" chann str))
                             urls "\n")))))))

;; (add-hook 'erc-insert-modify-hook 'my-erc-scan-urls)

(setq erc-log-p t)

(provide 'nav-erc)
;;; nav-erc.el ends here
