;; Set Windows-specific preferences if running in a Windows environment.
(defun to/browse-url-win (url &optional new-window)
  (shell-command
   (concat "start chrome " url)))

(defun to/windows-setup () (interactive)
       (setq git-shell-path (concat "C:\\Program Files\\Git\\bin")
             git-shell-executable (concat git-shell-path "\\bash.exe"))

       ;; Disable lockfiles to make flask spaz less
       (setq create-lockfiles nil)

       (add-to-list 'exec-path git-shell-path)
       (add-to-list 'exec-path "C:/Users/Tom/.cargo/bin")
       (setenv "PATH" (concat git-shell-path ";" (getenv "PATH") ";C:/Users/Tom/.cargo/bin" ))

       (setq browse-url-browser-function 'to/browse-url-win)

       (when (boundp 'w32-pipe-read-delay)
         (setq w32-pipe-read-delay 0))
       ;; Set the buffer size to 64K on Windows (from the original 4K)

       (when (boundp 'w32-pipe-buffer-size)
         (setq w32-pipe-buffer-size (* 64 1024)
               irony-server-w32-pipe-buffer-size (* 64 1024)))

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
(setq vc-make-backup-files t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      bookmark-default-file  (concat user-emacs-directory "bookmarks.em")
      bookmark-save-flag 1)


;;
;; Recent files
;;
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(add-to-list 'recentf-exclude "/elpa/")
(add-to-list 'recentf-exclude "company-statistics-cache.el")
(add-to-list 'recentf-exclude "bookmarks.em")
(add-to-list 'recentf-exclude ".mc-lists.el")
(add-to-list 'recentf-exclude "custom.el")

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

;;
;; User setup
;;
(setq user-full-name "Tom Olsson"
      user-mail-address "mail@tomolsson.se")

(use-package ctags-update
  :config
  (add-to-list 'ctags-update-other-options "--exclude=Build"))

(use-package helm-etags-plus
  :ensure t)

(provide 'basic-setup)
