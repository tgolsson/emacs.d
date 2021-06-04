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
                                        ;       (setenv "PATH" (concat git-shell-path ";" (getenv "PATH") ";C:/Users/Tom/.cargo/bin" ))

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
             browse-url-generic-program "firefox"))

(if (eq system-type 'windows-nt)
    (to/windows-setup)
  (to/other-setup))

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      bookmark-default-file  (concat user-emacs-directory "bookmarks.em")
      bookmark-save-flag 1
      ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain
      gc-cons-threshold 50000000
      history-length 1000
      large-file-warning-threshold 100000000
      user-full-name "Tom Solberg"
      user-mail-address "me@sbg.dev"
      vc-make-backup-files t
      compilation-scroll-output 'first-error
      locale-coding-system 'utf-8)

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package savehist
  :init
  (savehist-mode +1)
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60))

(use-package recentf
  :init
  (setq recentf-max-saved-items 2000)
  (recentf-mode +1)
  :config
  (to/append-to-list 'recentf-exclude '("/elpa/" "company-statistics-cache.el" "bookmarks.em" ".mc-lists.el" "custom.el")))


(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
  :init (setq save-place-file (concat user-emacs-directory "places"))
  :config (save-place-mode))

(use-package editorconfig :ensure t)

(provide 'basic-setup)
