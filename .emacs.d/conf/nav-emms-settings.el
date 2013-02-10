;; Activate nav-emms-mode-line in all emms buffers.
(dolist (buffer (list emms-lyrics-buffer emms-playlist-buffer emms-browser-buffer))
  (with-current-buffer buffer
    (nav/emms-mode-line 1)))

(provide 'nav-emms-settings)
;; nav-emms-settings.el ends here
