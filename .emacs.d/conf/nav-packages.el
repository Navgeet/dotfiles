;; ------------------
;; Package Management
;; ------------------

(require 'cl)
(require 'package)
;; add MELPA to package repositories
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; required because of a package.el bug
(setq url-http-attempt-keepalives nil)

(defvar nav/packages
  '(magit melpa ido-ubiquitous nrepl paredit rainbow-delimiters smex markdown-mode lua-mode clojure-mode)
  "A list of packages to ensure are installed at launch.")

(defun nav/packages-installed-p ()
  (loop for p in nav/packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun nav/install-packages ()
  (unless (nav/packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p nav/packages)
      (unless (package-installed-p p)
        (package-install p)))))

(nav/install-packages)

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(provide 'nav-packages)
;;; nav-packages.el ends here
