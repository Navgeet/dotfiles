;; -----------------------
;; Libraries and load path
;; -----------------------

;; add ~/.emacs.d/lisp directory to the start of load path
;; any library there will be loaded in place of the respective default
(let ((default-directory "~/.emacs.d/lisp/"))
      (setq load-path
            (append
             (let ((load-path (copy-sequence load-path))) ;; Shadow
               (normal-top-level-add-subdirs-to-load-path))
             load-path)))

;; add all directories under ~/.emacs.d recursively to the
;; end of load-path
(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

;;; Dependencies
;; List of files in the ~/.emacs.d/conf directory to load (order matters)
(let ((conf-files '(nav-packages
		    nav-emms
		    nav-lyrics
		    nav-elscreen
		    nav-emms-mode-line
		    nav-emms-settings
		    nav-erc)))
  (dolist (file conf-files) (require file)))


;; -------- 
;; Movement
;; --------

;; TODO: Add keybindings to scroll the "other" window in both directions
;; TODO: turn on subword mode always and global bind C-arrow keys to M-f/b
;;       this helps to work on words WithCamelCaseWritingLikeThis

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
;; TODO: What are these for?
;; (global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
;; (global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; ------- 
;; Windows
;; -------


;; ------- 
;; Editing
;; -------

;; auto-indent the point on pressing RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; ---------- 
;; Appearance
;; ----------

;; hide the splash screen and message in the echo area
(setq inhibit-startup-message t
inhibit-startup-echo-area-message t)

;; add ~/.emacs.d/themes/ subdirectories to custom-theme-load-path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
;; use solarized-dark theme as the default
(load-theme 'solarized-dark t)

;; font configuration
(set-default-font "Inconsolata-10")

;; Remove toolbar, menubar, scrollbar and tooltips
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-scroll-bar-mode 'nil)

;; show-paren-mode
(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'expression) ; alternatives are 'parenthesis' and 'mixed'

;; --------
;; Ido Mode
;; --------

(setq ido-enable-flex-matching t)
(ido-mode 1)
(setq ido-everywhere t)
;; never prompt when creating a buffer that does not exist
(setq ido-create-new-buffer 'always)

(ido-ubiquitous-mode 1)
;; -------------
;; Miscellaneous
;; -------------

;; keep backup files neatly out of the way in .~/
(setq backup-directory-alist '(("." . ".~")))
;; getting rid of the “yes or no” prompt and replace it with “y or n”
(fset 'yes-or-no-p 'y-or-n-p)
;; disable prompt when opening a file or buffer that does not exist
(setq confirm-nonexistent-file-or-buffer nil)

;; --------
;; Org Mode
;; --------

(setq org-replace-disputed-keys t)
(setq org-return-follows-link t)

;; -------
;; Paredit
;; -------

(defvar nav/paredit-mode-target-mode-list '(clojure-mode-hook
					    nrepl-mode-hook)
  "A list of target modes to which Paredit mode is hooked to")
(dolist (mode nav/paredit-mode-target-mode-list)
  (add-hook mode 'paredit-mode)
  (add-hook mode 'rainbow-delimiters-mode))

;; -----
;; nrepl
;; -----

;; Enable eldoc in clojure buffers
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)
;; Make C-c C-z switch to the *nrepl* buffer in the current window
(add-to-list 'same-window-buffer-names "*nrepl*")
;; Enabling CamelCase support
(add-hook 'nrepl-mode-hook 'subword-mode)

;; ----
;; smex
;; ----

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq smex-key-advice-ignore-menu-bar t)

;; ----
;; Rest
;; ----

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("0bac11bd6a3866c6dee5204f76908ec3bdef1e52f3c247d5ceca82860cccfa9d" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
