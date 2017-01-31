;; Set Windows-specific preferences if running in a Windows environment.
(defun to/browse-url-win (url &optional new-window)
  (shell-command
   (concat "start chrome " url)))

(defun to/windows-setup () (interactive)
       (setq git-shell-path (concat "C:\\Program Files\\Git\\bin"))
       (setq git-shell-executable (concat git-shell-path "\\bash.exe"))
       (add-to-list 'exec-path git-shell-path)
       (setenv "PATH" (concat git-shell-path ";" (getenv "PATH")))

       (setq browse-url-browser-function 'to/browse-url-win)

       (when (boundp 'w32-pipe-read-delay)
         (setq w32-pipe-read-delay 0))
       ;; Set the buffer size to 64K on Windows (from the original 4K)
       (when (boundp 'w32-pipe-buffer-size)
         (setq w32-pipe-buffer-size (* 64 1024))
         (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
       (message "Windows preferences set."))

(defun to/other-setup () (interactive)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

(if (eq system-type 'windows-nt)
    (to/windows-setup)
  (to/other-setup))

;;
;; Commands
;; 
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;;
;; Backups, bookmarks and save-place
;;
(setq vc-make-backup-files t ;; Enable backups for git-files etc
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      bookmark-default-file  (concat user-emacs-directory "bookmarks.em")
      bookmark-save-flag 1)


;;
;; Recent files
;;
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent
(add-to-list 'recentf-exclude "/elpa/")
(add-to-list 'recentf-exclude "company-statistics-cache.el")
(add-to-list 'recentf-exclude "bookmarks.em")

;;
;; Minibuffer-history
;;
(savehist-mode 1)
(setq history-length 1000)

;;
;; A saner ediff
;;
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(setq gc-cons-threshold 50000000
      large-file-warning-threshold 100000000)
(fset 'yes-or-no-p 'y-or-n-p)




(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
            :config
            (setq save-place t)
            save-place-file (concat user-emacs-directory "places"))


(provide 'basic-setup)
