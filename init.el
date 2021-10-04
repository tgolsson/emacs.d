
;; PRE-INIT
(setq gc-cons-threshold 500000000 package-enable-at-startup nil
      load-prefer-newer t
      use-package-always-ensure t use-package-always-defer t)

(defun to/report-startup-time ()
  (message "Emacs ready in %s with %d garbage collections." (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done))

(add-hook 'emacs-startup-hook 'to/report-startup-time)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

; (package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
; (unless package-archive-contents
;   (package-refresh-contents)
;)

;; Initialize use-package on non-Linux platforms
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

; (require 'use-package)

(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))


(define-coding-system-alias 'cp65001 'utf-8)
(prefer-coding-system 'utf-8) ;; fixes some packages containing non-iso-latin characets
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq settings-dir (expand-file-name "settings" user-emacs-directory)
      experiments-dir (expand-file-name "experiments" user-emacs-directory)
      packages-dir (expand-file-name "packages" user-emacs-directory)
      experiments-dir (expand-file-name "experiments" user-emacs-directory)
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      cursor-type t
      font-lock-multiline t
      frame-background-mode 'dark
      inhibit-splash-screen t
      inhibit-startup-message t
      linum-delay t
      linum-eager t
      linum-format "%4d  "
      scroll-conservatively 100000
      scroll-margin 0
      scroll-preserve-screen-position 0
      indent-tabs-mode nil
      fill-column 80
      frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

(setq-default font-lock-multiline t
              custom-safe-themes t
              custom-theme-directory (expand-file-name "themes" user-emacs-directory))

(add-to-list 'load-path experiments-dir)
(add-to-list 'load-path packages-dir)
(add-to-list 'load-path (expand-file-name "emacs-bazel-mode" packages-dir))

(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(mapc 'frame-set-background-mode (frame-list))

(use-package benchmark-init
  :demand t
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; (use-package auto-package-update
;;   :custom
;;   (auto-package-update-interval 7)
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "09:00"))

(use-package no-littering)

(use-package
  exec-path-from-shell
  ;; :when (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))
;; (when window-system (set-exec-path-from-shell-PATH))

(defun risky-local-variable-p (sym &optional _ignored) nil)

(defmacro measure-time (msg &rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time))) ,@body (message "%s %.06f" ,msg (float-time (time-since time)))))

(defun to/append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error
     "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(defun to/do-list-dir (thedir)
  "Iterates over all files in THEDIR and loads them"
  (if (file-accessible-directory-p thedir)
      (progn (dolist (file (directory-files thedir t "\.el$" nil))
               (load (file-name-sans-extension file) t t)))))

(use-package ag)
(use-package anzu)
(use-package yasnippet
  :commands (yas-global-mode)
  :config
  (yas-global-mode))
(use-package speed-type)
(use-package dumb-jump)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (set-face-attribute 'mode-line nil :underline nil)
  :config
  (set-face-attribute 'mode-line nil :underline nil)
  (when (not (memq window-system '(w32))) (all-the-icons-install-fonts t)))

(use-package dap-mode
  :commands dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :init
  (use-package treemacs)
  :config
  ;; (require 'dap-node)
  ;; (dap-node-setup)
  (require 'dap-go)
  (dap-go-setup)
  (require 'dap-hydra)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
				     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil))
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra)))


(add-hook 'prog-mode-hook (lambda () "" (interactive) (when (window-system) (cascadia-code-mode))))

(when window-system (set-frame-font "Cascadia Code 12"))

(blink-cursor-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(set-fringe-mode 20)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-major-tick 50
      display-line-numbers-minor-tick 10)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package paren
  :commands show-paren-mode
  :init
  (show-paren-mode +1))

(use-package solarized-theme
  :demand t
  :config
  (load-theme 'solarized-light t)
  (enable-theme 'solarized-light))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config (defun to/set-emoji-font (frame)
            "Adjust the font settings of FRAME so Emacs can display emoji properly."
            (if (eq system-type 'darwin)
                ;; For NS/Cocoa
                (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
              ;; For Linux
              (set-fontset-font t 'symbol (font-spec :family "Symbola") frame
                                'prepend)))
  (to/set-emoji-font nil)
  (add-hook 'after-make-frame-functions 'to/set-emoji-font))

(use-package hl-line
  :demand t
  :init (global-hl-line-mode +1))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package smooth-scrolling)
(use-package rotate)
(use-package copy-as-format)
(use-package gif-screencast)

(use-package lsp-mode
  :commands (lsp ls-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-prefer-flymake nil
        lsp-prefer-capf t
        lsp-ui-doc-use-childframe nil)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind-keymap ("C-c C-l" . lsp-command-map)
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t)
  (setq lsp-go-env (make-hash-table :test 'equal))
  (puthash "GOPROXY" "https://proxy.golang.org,direct" lsp-go-env))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(defun to/browse-url-win (url &optional new-window)
  (shell-command (concat "start chrome " url)))

(defun to/windows-setup ()
  (interactive)
  (setq git-shell-path (concat "C:\\Program Files\\Git\\bin")
        git-shell-executable (concat
                              git-shell-path "\\bash.exe"))

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
    (setq w32-pipe-buffer-size (* 64 1024) irony-server-w32-pipe-buffer-size (* 64 1024)))
  (message "Windows preferences set."))

(defun to/other-setup ()
  (interactive)
  (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program
        "firefox"))

(if (eq system-type 'windows-nt)
    (to/windows-setup)
  (to/other-setup))


(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation
buffer if succeeded without warnings "
  (if (and (string-match "compilation"
                         (buffer-name
                          buffer))
           (string-match "finished" string)
           (not (with-current-buffer buffer (search-forward "warning" nil t))))
      (run-with-timer 1 nil (lambda (buf)
                              (bury-buffer buf)
                              (switch-to-prev-buffer (get-buffer-window buf) 'kill)) buffer)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      bookmark-default-file  (concat user-emacs-directory "bookmarks.em")
      bookmark-save-flag 1
      ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain
      history-length 1000
      large-file-warning-threshold 100000000
      user-full-name "Tom Solberg"
      user-mail-address "me@sbg.dev"
      vc-make-backup-files t
      compilation-scroll-output 'first-error locale-coding-system 'utf-8)

(use-package
  savehist
  :init (savehist-mode +1)
  :config (setq savehist-additional-variables '(search-ring regexp-search-ring)
                savehist-autosave-interval 60))

(use-package
  recentf
  :bind (("<f6>" . counsel-recentf))
  :init (setq recentf-max-saved-items 2000)
  (recentf-mode +1)
  :config
  (to/append-to-list 'recentf-exclude '("/elpa/"
                                        "company-statistics-cache.el"
                                        "bookmarks.em" ".mc-lists.el"
                                        "custom.el"
                                        no-littering-etc-directory
                                        no-littering-var-directory)))

(straight-use-package
  '(uniquify :type built-in)
  :config (setq uniquify-buffer-name-style 'forward))

(use-package
  saveplace
  :init (setq save-place-file (concat user-emacs-directory "places"))
  :config (save-place-mode))

(use-package
  editorconfig
  :ensure t)

;;
;; Transient mark mode
;;
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;;
;; General input modes
;;
(global-subword-mode +1)

(use-package
  elec-pair
  :commands elec-pair-mode
  :hook (prog-mode . electric-pair-mode)
  :demand t
  :config (electric-pair-mode +1))

;;
;; General input setqs
;;
(setq echo-keystrokes 0.1 x-select-enable-clipboard t x-select-enable-primary t
      save-interprogram-paste-before-kill t apropos-do-all t mouse-yank-at-point
      t
      require-final-newline t fill-column 80 delete-selection-mode t confirm-kill-emacs 'y-or-n-p)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;;
;; General input setqs
;;
(setq-default )

(use-package
  multiple-cursors
  :bind (("M-ö" .    mc/edit-lines)
         ("C-M-ö" .  mc/edit-ends-of-lines)
         ("M-Ö" .    mc/edit-beginnings-of-lines)
         ("M-ä" .    mc/mark-all-dwim)
         ("C-<" .    mc/mark-previous-like-this)
         ("C->" .    mc/mark-next-like-this)
         ("C-S-ä" .  mc/mark-more-like-this-extended)
         ("M-å" .    mc/mark-all-in-region))
  :config
  (unsupported-cmd isearch-forward-use-region ".")
  (unsupported-cmd isearch-backward-use-region "."))

(use-package
  which-key
  :init (which-key-mode 1)
  :config (setq which-key-idle-delay 0.3))

(use-package
  autoinsert
  :init (setq auto-insert-directory (expand-file-name "templates" user-emacs-directory))
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  (define-auto-insert "\\.el$" ["default-lisp.el" to/autoinsert-yas-expand])
  (define-auto-insert "\\.py$" ["default-python.py" to/autoinsert-yas-expand])
  (define-auto-insert "\\.rs$" ["default-rust.rs" to/autoinsert-yas-expand])
  (define-auto-insert "/sprints/" ["sprint.org" to/autoinsert-yas-expand])
  (define-auto-insert "test_.*.py" ["test.py" to/autoinsert-yas-expand])
  ;; autoinsert C/C++ header
  (define-auto-insert (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
    '(nil "// " (file-name-nondirectory buffer-file-name) "\n" "//\n" "//
  Copyright © Tom Solberg\n"
          "//\n" "// Description:\n" "//\n" (make-string 70 ?/) "\n\n" "//
  last-edit-by: <"
          (user-full-name) "> \n" "// $Log:$\n" "//\n" (make-string 70 ?/)
          "\n\n" "#pragma
once\n\n"))

  ;; auto insert C/C++
  (define-auto-insert (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++
  implementation")
    '(nil "// " (file-name-nondirectory buffer-file-name) "\n" "//\n" "// Copyright
  © Tom Solberg\n" "// \n" "// Description:\n" "//\n" (make-string 70 ?/) "\n\n"
  "// last-edit-by: <" (user-full-name) "> \n" "// $Log:$\n" "//\n" (make-string
                                                                     70 ?/)
  "\n\n" "#include \""
  (concat (file-name-sans-extension
           (file-name-nondirectory buffer-file-name)) ".h") "\"")))

(setq comint-prompt-read-only t)
;; (use-package hideshow
;;   :config
;;   (define-key hs-minor-mode-map (kbd "C-c h") (lookup-key hs-minor-mode-map (kbd "C-c @")))
;;   (define-key hs-minor-mode-map (kbd "C-c @") nil))

(defun to/preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))
(add-hook 'comint-preoutput-filter-functions 'to/preoutput-turn-buffer-read-only)

;; (use-package expand-region
;;   :ensure t
;;   :bind ("C-@" . er/expand-region))

(require 'cascadia-code)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :ensure t
  :defer 10
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (use-package ibuffer-projectile
    :hook (ibuffer . (lambda ()
                       (ibuffer-projectile-set-filter-groups)
                       (unless (eq ibuffer-sorting-mode 'alphabetic)
                         (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq projectile-switch-project-action 'magit-status
        projectile-globally-ignored-directories (append '("*__pycache__*" "*.egg-info") projectile-globally-ignored-directories)
        projectile-globally-ignored-file-suffixes (append '(".pyc") projectile-globally-ignored-file-suffixes)
        projectile-indexing-method 'alien
        projectile-enable-caching 't
        projectile-git-command "fd . -0"
        projectile-git-submodule-command "git submodule --quiet foreach 'echo $path'")
  (projectile-mode +1)
  (projectile-global-mode 1)
  ;;(helm-projectile-on)
  )

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; (use-package helm-projectile
;;   :ensure t
;;   :demand t
;;   :config
;;   (progn
;;     (setq projectile-completion-system 'helm)
;;     (helm-projectile-on)
;;     )
;;   )


(eval-when-compile
  (require 'cl))

(defun untabify-buffer ()
  (interactive)
  (untabify 1 (point-max))
  (if (not (eq major-mode 'mew-draft-mode))
      ;; delete-trailing-whitespace does not work in mew-draft-mode.
      (delete-trailing-whitespace)))


(defun rename-current-buffer-file ()
  "Renames current buffer and file it is
visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename
                  (file-exists-p filename)))
        (error
         "Buffer '%s' is not visiting a file!"
         name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error
             "A buffer named '%s' already exists!"
             new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory
                                                                  new-name)))))))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))


(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or
{p, p, p} or [p, p, p]"
  (interactive)
  (let* ((end-of-first (cond ((looking-at ", ")
                              (point))
                             ((and
                               (looking-back ",")
                               (looking-at " "))
                              (- (point) 1))
                             ((looking-back ", ")
                              (- (point) 2))
                             (t
                              (error
                               "Place point between params to transpose."))))
         (start-of-first (save-excursion (goto-char end-of-first)
                                         (move-backward-out-of-param)
                                         (point)))
         (start-of-last (+ end-of-first 2))
         (end-of-last (save-excursion (goto-char start-of-last)
                                      (move-forward-out-of-param)
                                      (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))


;; shorthand for interactive lambdas
(defmacro λ
    (&rest
     body)
  `(lambda ()
     (interactive)
     ,@body))


(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward))


(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward))


(defun check-expansion ()
  (save-excursion (if (looking-at "\\_>") t (backward-char 1)
                      (if (looking-at "\\.") t (backward-char 1)
                          (if (looking-at "->") t nil)))))


(defun to/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas/expand-snippet (buffer-string)
                      (point-min)
                      (point-max)))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))


(defun tab-indent-or-complete ()
  (interactive)
  (if (or (not (or yas-global-mode
                   yas/minor-mode))
          (null (do-yas-expand)))
      (if (check-expansion)
          (company-complete-common)
        (indent-for-tab-command))))


(defun to/kill-other-buffers
    (&optional
     kill-special)
  "Kill buffers that do not belong to a `projectile' project.

With prefix argument (`C-u'), also kill the special buffers."
  (interactive "P")
  (let ((bufs (buffer-list (selected-frame))))
    (dolist (buf bufs)
      (with-current-buffer buf (let ((buf-name (buffer-name buf)))
                                 (when (or (null (projectile-project-p))
                                           (and kill-special
                                                (string-match "^\*" buf-name)))
                                   ;; Preserve buffers with names starting with *scratch or
                                   ;; *Messages
                                   (unless (string-match "^\\*\\(\\scratch\\|Messages\\)" buf-name)
                                     (message "Killing buffer %s" buf-name)
                                     (kill-buffer buf))))))))


(defun to/close-older-buffers ()
  ;; Run in interactive
  (interactive)
  ;; let bufs -> reverse buffer list (oldest first)
  ;;     numbufs -> number of buffs in buffer-list
  (let ((bufs (reverse (buffer-list (selected-frame))))
        (numbufs (length (buffer-list (selected-frame)))))
    ;; for each buffer
    (dolist (buf bufs)
      ;; if there's more than 10 buffers in list
      (if (> numbufs 10)
          ;; let current buffer be buf
          (with-current-buffer buf (progn
                                     ;; extract the buffer-name for easier use
                                     (setq buf-name (buffer-name buf))
                                     ;; if buffer-name begins with star
                                     (if (string-match "\*" buf-name)
                                         ;; reduce remaining buffers by 1
                                         (setq numbufs (- numbufs 1))
                                       ;; else
                                       (progn
                                         ;; if modified, prompt before killing
                                         (if (buffer-modified-p buf)
                                             (if (y-or-n-p (concat "Kill unsaved
buffer " buf-name
"?"))
                                                 ;; if yes
                                                 (progn
                                                   ;; kill and reduce ounter
                                                   (kill-buffer buf)
                                                   (setq numbufs (- numbufs 1))))
                                           ;; else kill directly and reduce counter
                                           (progn (kill-buffer)
                                                  (setq numbufs (- numbufs 1))))))))))))


(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun write-region-delete-and-open(start end filename)
  "function takes current
region, and writes it to specified file"
  (interactive "r\nFFilename: ")
  (write-region start end filename t)
  (kill-region start end))


(defun random-uuid ()
  "Insert a UUID. This uses a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

Note: this code uses https://en.wikipedia.org/wiki/Md5"
  (interactive)
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s" (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))
    (insert (format "%s-%s-4%s-%s%s-%s" (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
                    (format "%x" (+ 8 (random 4)))
                    (substring myStr 17 20)
                    (substring myStr 20 32)))))

(defun* yank-arg () "Yanks the arg at point, abiding by the syntax-table of the
current mode"
        (interactive)
        (let (bounds start end)
          (setq bounds (bounds-of-thing-at-point 'symbol))
          (if bounds (progn
                       (setq start (car bounds))
                       (setq end (cdr bounds)))
            (cond ((eq (char-syntax (char-after)) ?\()
                   (progn
                     (setq start (point))
                     (forward-sexp)
                     (setq end (point))))
                  ((eq (char-syntax (char-before)) ?\()
                   (progn (backward-char)
                          (setq start (point))
                          (forward-sexp)
                          (setq end (point))))
                  ((eq (char-syntax (char-before)) ?\))
                   (progn
                     (setq end (point))
                     (backward-sexp)
                     (setq start (point))))
                  (t (progn (message "No arg at point to yank!")
                            (return-from yank-arg)))))
          (goto-char end)
          (cond ((eq (char-syntax (char-after)) ?\() ; Followed by a sexp
                 (progn (forward-sexp)
                        (setq end (point))))
                ((looking-at "[ ,]") ; Whitespace or comma - zap until next symbol
                 (while (looking-at "[ ,]")
                   (forward-char))
                 (setq end (point))))
          (goto-char start)
          (while (looking-back "[[:space:],\n]")
            (backward-char))
          (setq start (point))
          (kill-region start end)))

(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn (window-configuration-to-register '_)
           (delete-other-windows))))

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond ((point-is-in-string-p)
           (move-point-forward-out-of-string))
          ((looking-at "(\\|{\\|\\[")
           (forward-list))
          (t (forward-char)))))

(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond ((point-is-in-string-p)
           (move-point-backward-out-of-string))
          ((looking-back ")\\|}\\|\\]")
           (backward-list))
          (t (backward-char)))))

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun to/regen-rusty-tags-projectile ()
  "generate tags files for all rust
projects below the dominating .projectile file"
  (interactive)
  (start-process "rusty-tags"   ;; process-name
                 "*rusty-tags*" ;; buffer-name
                 "rusty-tags"   ;; executable name

                 ;; rest = args
                 "-o" "-s" (expand-file-name (locate-dominating-file
                                              default-directory
                                              ".projectile"))
                 ;; generate for emacs
                 "emacs"))

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore
notation for the symbol at point."
  (interactive)
  (save-excursion (let* ((bounds (bounds-of-thing-at-point 'symbol))
                         (start (car bounds))
                         (end (cdr bounds))
                         (currently-using-underscores-p (progn (goto-char start)
                                                               (re-search-forward "_" end t))))
                    (if currently-using-underscores-p (progn (upcase-initials-region start end)
                                                             (replace-string "_" "" nil start end)
                                                             (downcase-region start (1+ start)))
                      (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
                      (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

;; C, C++, Ardunio
(add-to-list  'auto-mode-alist '("\\.as$"   . c++-mode))
(add-to-list  'auto-mode-alist '("\\.h$"   . c++-mode))
(add-to-list  'auto-mode-alist '("\\.hpp$"   . c++-mode))
(add-to-list  'auto-mode-alist '("\\.cpp$"   . c++-mode))
(add-to-list  'auto-mode-alist '("\\.inl$"   . c++-mode))
(add-to-list  'auto-mode-alist '("\\.c$"   . c-mode))
(add-to-list 'auto-mode-alist '("\.ino$" . arduino-mode))
;; (add-to-list 'auto-mode-alist '("\.rs$" . rust-mode))

;; WEB
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs" . web-mode))
(add-to-list 'auto-mode-alist '("\\.svelte" . web-mode))
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; OTHER MODES
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;; Work
(add-to-list 'auto-mode-alist '("\\.build\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("meson.build\\'" . meson-mode))


;;                magit-todos

;;
;; magit
;;
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status))
  :bind (:map magit-status-mode-map ( "q" . magit-quit-session))
  :init
  ;; (use-package magit-filenotify
  ;;   :hook (magit-status-mode . magit-filenotify-mode))
  ;; (use-package magit-todos
  ;;   :hook (magit-status-mode . magit-todos-mode))
  :config
  (custom-set-faces '(magit-diff-added ((t (:background "black" :foreground "green3"))))
                    '(magit-diff-removed ((t (:background "black" :foreground "red3"))))))

(use-package gitignore-mode)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(use-package company
  :after lsp-mode
  :hook ((lsp-mode . company-mode))
  :commands company-mode
  :init
  (use-package company-statistics
    :hook (company-mode . company-statistics-mode))
  (use-package company-quickhelp
    :hook (company-mode . company-quickhelp-mode))
  (setq company-tooltip-limit 20
        company-tooltip-align-annotations 't
        company-idle-delay .1
        company-begin-commasends '(lf-insert-command)
        company-minimum-prefix-length 1
	company-backends '(company-capf))
  :config (define-key company-active-map (kbd "TAB") 'tab-indent-or-complete)
  (define-key company-active-map (kbd "<tab>") 'tab-indent-or-complete))

;;                flycheck
;;                flycheck-clang-tidy
;;                flycheck-golangci-lint
;;                flycheck-inline
;;                flycheck-irony
;;                flycheck-pos-tip
;;                flycheck-rust
;;                flymake-gjshint
;;                flymake-rust
;;                flymake-sass

(defun to/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay (if flycheck-current-errors 0.5 30.0)))

(defun flycheck-handle-idle-change ()
  "Handle an expired idle time since the last change.
This is an overwritten version of the original
flycheck-handle-idle-change, which removes the forced deferred.
Timers should only trigger inbetween commands in a single
threaded system and the forced deferred makes errors never show
up before you execute another command."
  (flycheck-clear-idle-change-timer)
  (flycheck-buffer-automatically 'idle-change))


(use-package
  flycheck
  :commands flycheck-mode
  :init (use-package
          flycheck-clang-tidy
          :ensure t
          :hook (cc-mode . flycheck-clang-tidy-setup)
          :after flycheck)
  (use-package
    flycheck-pos-tip
    :hook (flycheck-mode-hook . flycheck-pos-tip-mode)
    :config (custom-set-variables '(flycheck-display-errors-function
                                    #'flycheck-pos-tip-error-messages))
    (make-variable-buffer-local 'flycheck-idle-change-delay)
    :config (setq-default
             flycheck-disabled-checkers
             '(python-pylint))
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)))
  (add-hook 'flycheck-after-syntax-check-hook
            'to/adjust-flycheck-automatic-syntax-eagerness))

;; helm
;; helm-ag
;; helm-core
;; helm-descbinds
;; helm-describe-modes
;; helm-etags-plus
;; helm-flx
;; helm-flycheck
;; helm-flymake
;; helm-flyspell
;; helm-fuzzier
;; helm-ls-git
;; helm-projectile
;; helm-smex
;; helm-swoop
;; helm-w32-launcher


;; (use-package
;;     helm
;;     :ensure t
;;     :init (use-package
;;               helm-swoop
;;               :ensure t
;;               :bind
;;               (("M-s" . helm-swoop)
;;                   ("C-x M-s" . helm-swoop)))

;;     (use-package
;;         helm-descbinds
;;         :ensure t
;;         :bind (("C-h b" . helm-descbinds)))
;;     (use-package
;;         helm-flycheck
;;         :ensure t
;;         :bind (:map flycheck-mode-map
;;                   ("C-c ! h" . helm-flycheck)))
;;     (use-package
;;         helm-describe-modes
;;         :ensure t)
;;     (use-package
;;         helm-ls-git
;;         :ensure t
;;         :bind ( ("C-x M-g" . helm-ls-git-ls)))
;;     (use-package
;;         helm-smex
;;         :ensure t
;;         :bind (("<remap> <execute-extended-command>"
;;                    . helm-smex)
;;                   ("M-X". helm-smex-major-mode-commands)))
;;     :bind (("C-x C-f" . helm-find-files)
;;               ("C-x b" . helm-mini)
;;               ("M-y" . helm-show-kill-ring)
;;               ("C-h a" . helm-apropos)
;;               ("C-h i" . helm-info-emacs)
;;               ("<f6>" . helm-recentf)
;;               ("C-x r l" . helm-filtered-bookmarks)
;;               ("C-x c!" . helm-calcul-expression))
;;     :config (define-key helm-map (kbd "<left>")
;;                 'helm-previous-source)
;;     (define-key helm-map (kbd "<right>") 'helm-next-source)
;;     (setq helm-mode-fuzzy-match t helm-buffers-fuzzy-matching t
;;         helm-recentf-fuzzy-match    t helm-ff-transformer-show-only-basename t
;;         helm-move-to-line-cycle-in-source t helm-ff-keep-cached-candidates nil
;;         helm-ff-auto-update-initial-value t helm-ag-insert-at-point 'symbol)
;;     (helm-mode t)
;;     (helm-adaptive-mode t)
;;     ;; for helm-find-files
;;     (customize-set-variable 'helm-ff-lynx-style-map t)

;;     ;; for helm-imenu
;;     (customize-set-variable 'helm-imenu-lynx-style-map t)

;;     ;; for semantic
;;     (customize-set-variable 'helm-semantic-lynx-style-map t)

;;     ;; for helm-occur
;;     (customize-set-variable 'helm-occur-use-ioccur-style-keys t)

;;     ;; for helm-grep
;;     (customize-set-variable 'helm-grep-use-ioccur-style-keys t)
;;     (use-package
;;         helm-flx
;;         :config (helm-flx-mode +1)
;;         (setq helm-flx-for-helm-find-files t helm-flx-for-helm-locate t
;;             helm-buffer-file-name nil)))

(use-package ivy
  :diminish
  :bind (("M-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-virtual-buffers t
	ivy-initial-inputs-alist '((counsel-minor . "^+")
				   (counsel-package . "^+")
				   (counsel-org-capture . "^")
				   (counsel-M-x . "")
				   (counsel-describe-symbol . "^")
				   (org-refile . "^")
				   (org-agenda-refile . "^")
				   (org-capture-refile . "^")
				   (Man-completion-table . "^")
				   (woman . "^")))
  (ivy-mode 1)
  :init
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))


  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))


(global-set-key (kbd "<f7>")                 'ctags-create-or-update-tags-table)

(global-set-key (kbd "M-/")                  'hippie-expand)
(global-set-key (kbd "C-x C-b")              'ibuffer)
(global-set-key (kbd "M-z")                  'zap-up-to-char)

(global-set-key (kbd "C-s")                  'isearch-forward-regexp)
(global-set-key (kbd "C-r")                  'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")                'isearch-forward)
(global-set-key (kbd "C-M-r")                'isearch-backward)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key (kbd "C-x C-r")              'rename-current-buffer-file)
;; (global-set-key [remap goto-line]            'goto-line-with-feedback)

;; Perform general cleanup.
(global-set-key (kbd "C-c n")                'cleanup-buffer)
(global-set-key (kbd "C-c C-n")              'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>")       'delete-blank-lines)

;; DMM
(global-set-key (kbd "C-h C-m")              'discover-my-major)
(global-set-key (kbd "C-h M-m")              'discover-my-mode)

;; Transpose
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l")                'transpose-lines)
(global-set-key (kbd "M-t w")                'transpose-words)
(global-set-key (kbd "M-t s")                'transpose-sexps)
(global-set-key (kbd "M-t p")                'transpose-params)

(global-set-key (kbd "C-c u")                'uncomment-region)
(global-set-key (kbd "C-c c")                'comment-or-uncomment-region)

(global-set-key (kbd "M-p")                  'backward-paragraph)
(global-set-key (kbd "M-n")                  'forward-paragraph)
;;(global-set-key [tab]                      'tab-indent-or-complete)

(global-set-key (kbd "M-i") 'helm-etags-select)
;; programming mode
(define-key prog-mode-map (kbd "M-W") 'yank-arg)

(bind-key "M-Q" 'delete-trailing-whitespace)
;; Make commit-lines 72 lines max

(add-hook 'git-commit-mode-hook (lambda ()
                                  (interactive "")
                                  (set-fill-column 72)))

;;
;; Diminish
;;
(use-package
  diminish
  :demand t
  :defer 10
  :config (diminish 'auto-fill-function)
  (eval-after-load "minimap" '(diminish 'minimap-mode))
  (eval-after-load "guide-key" '(diminish 'guide-key-mode))
  (eval-after-load "abbrev" '(diminish 'abbrev-mode))
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
  (eval-after-load "auto-revert" '(diminish 'auto-revert-mode))
  (eval-after-load "flyspell" '(diminish 'flyspell-mode))
  (eval-after-load "flycheck" '(diminish 'flycheck-mode))
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "subword" '(diminish 'subword-mode))
  (eval-after-load "projectile" '(diminish 'projectile-mode))
  (eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
  (eval-after-load "fixme-mode" '(diminish 'fixme-mode))
  (eval-after-load "fira-code-mode" '(diminish 'fira-code-mode)))

;; Load modes
(measure-time "Loading all auxillaries:" (to/do-list-dir experiments-dir))
(load-file (expand-file-name "local-config.el" user-emacs-directory))

;;                clang-format
;;                company-c-headers
;;                modern-cpp-font-lock
;;
;; C-mode settings
;;
(use-package cc-mode
  :defer t
  :mode (("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.c\\'" . c-mode))
  :bind (:map c-mode-base-map ("RET" . newline-and-indent))
  :init
  (setq-default c-default-style "linux"
                c-basic-offset 4)
  :config
  (c-set-offset 'inline-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'topmost-intro 0)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'member-init-intro 0)
  (add-hook 'c-mode-common-hook
            (lambda () (interactive "")
              (progn (company-mode 1)
                     (irony-mode 1)
                     (company-irony 1)
                     (electric-pair-mode 1)
                     (flycheck-mode)
                     (make-local-variable 'company-backends)
                     (add-hook 'before-save-hook 'clang-format-buffer t t)
                     (add-to-list 'company-backends '
                                  (company-irony company-irony-c-headers
                                                 company-yasnippet)) )) ) )


;;
;; Rtags
;;
;; (use-package rtags
;;   :bind (( "M-." .  rtags-find-symbol-at-point)
;;          ( "M-," .  rtags-find-references-at-point)
;;          ( "C-." .  rtags-find-symbol)
;;          ( "C-," .  rtags-find-references)
;;          ( "C-<" .  rtags-find-virtuals-at-point)
;;          ( "M-i" .  rtags-imenu))
;;   :config
;;   (setq rtags-completions-enabled t))


;;
;; irony-mode
;;
(use-package irony
  :hook cc-mode
  :defer t
  :init
  (use-package company-irony-c-headers
    :commands company-irony-c-headers)
  (use-package company-irony
    :commands company-irony)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; Redefinition of function from gud.el
(defun gud-find-expr (&rest args)
  (let ((expr (if (and transient-mark-mode mark-active)
		  (buffer-substring (region-beginning) (region-end))
		(apply gud-find-expr-function args))))
    (save-match-data
      (if (string-match "\n" expr)
	  (error "Expression must not include a newline"))
      (with-current-buffer gud-comint-buffer
	(save-excursion
	  (goto-char (process-mark (get-buffer-process gud-comint-buffer)))
	  (forward-line 0)
	  (when (looking-at comint-prompt-regexp)
	    (set-marker gud-delete-prompt-marker (point))
	    (set-marker-insertion-type gud-delete-prompt-marker t))
	  (unless (eq (buffer-local-value 'gud-minor-mode gud-comint-buffer)
		      'jdb)
	    (message (concat expr " = "))))))
    expr))

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode))
  :config
  (use-package company-cmake
    :hook cmake-mode)
  (use-package cmake-font-lock
    :hook cmake-mode)
  :init
  (add-hook 'cmake-mode-hook '(lambda () (interactive "")
                                (make-local-variable 'company-backends)
                                (setq company-backends '(company-cmake company-yasnippet))
                                (company-mode 1)
                                (cmake-font-lock-activate))))

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p
              (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(straight-use-package
 '(lisp-mode
   :type built-in
   :mode ("\\.el\\'" . emacs-lisp-mode)
   :bind (:map emacs-lisp-mode-map ("\r" . reindent-then-newline-and-indent))
   :init
   (add-hook 'emacs-lisp-mode-hook
             '(lambda ()
		(abbrev-mode 1)
		(auto-fill-mode 1)
		(font-lock-mode 1)
		(company-mode 1)
		(message "WTF")
		(eldoc-mode 1)
		(flyspell-prog-mode)
		(make-local-variable 'company-backends)
		(add-to-list 'company-backends '(company-elisp company-yasnippet))
		(add-hook 'after-save-hook 'byte-compile-current-buffer nil t)))))

(use-package lua-mode
  :mode "\\.lua\\'"
  :init
  (use-package company-lua
    :commands company-lua)                          ; load company lua
  :config
  (company-mode 1)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-lua company-yasnippet)))

;; markdown-preview-eww
;; markdown-toc
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (use-package company-emoji
    :commands company-emoji)
  (use-package markdown-toc
    :commands (markdown-toc-generate-toc markdown-toc-refresh-toc))
  :config
  (abbrev-mode 1)
  (auto-fill-mode 0)
  (font-lock-mode 1)
  (add-to-list 'company-backends 'company-emoji)
  (add-to-list 'company-backends 'company-yasnippet))

(use-package go-mode
  :mode "\\.go\\'"
  :init
  ;; go-projectile
  ;; (use-package company-go :hook go-mode)
  (use-package flycheck-golangci-lint :commands flycheck-golangci-lint-setup)
  ;; (use-package go-eldoc :hook go-mode)
  ;; (use-package go-gopath :hook go-mode)
  ;; (use-package go-impl :hook go-mode)
  (use-package go-projectile :commands go-projectile-tools-add-path :hook (go-mode . go-projectile-set-gopath))
  :config
  (defun to/my-go-mode ()
    ;; (define-key go-mode-map (kbd "C-c C-e") #'go-gopath-set-gopath)
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    ;; (go-eldoc-setup)
    (company-mode 1)
    (go-projectile-tools-add-path)
    (make-local-variable 'company-backends)
    (set (make-local-variable 'company-backends) '(company-capf))
    (lsp)
    (lsp-mode 1)
    (flycheck-golangci-lint-setup)
    (flycheck-mode 1)
    (flycheck-pos-tip-mode 1)
    (flycheck-add-next-checker 'lsp 'golangci-lint)
    (add-hook 'before-save-hook 'gofmt-before-save nil t))
  (add-hook 'go-mode-hook 'to/my-go-mode))


;; (use-package cargo
;;   :ensure t
;;   :defer t)

;; (use-package helm-lsp
;;   :commands )

;; (use-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-mode
  :hook (rust-mode . lsp)
  :ensure t
  :mode "\\.rs\\'"
  :init
  :config
  (set (make-local-variable 'compile-command) "cargo run")
  (if (eq system-type 'windows-nt)
      (progn
        (add-to-list 'exec-path "C:/Users/Tom/.cargo/bin"))
    (progn
      (add-to-list 'exec-path "~/.cargo/bin")))

  (setq lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-chaining-hints t)
  (lsp 1)
  (lsp-mode 1)
  (lsp-lens-mode 1)
  (lsp-rust-analyzer-inlay-hints-mode 1)
  ;; (company-mode 1)
  ;; (cargo-minor-mode 1)
  (flycheck-mode 1)
  (hs-minor-mode 1)
  (flycheck-pos-tip-mode 0)
  (flycheck-inline-mode 0)
  (add-hook 'before-save-hook #'lsp-format-buffer t t))


(use-package protobuf-mode
  :mode "\\.proto\\'"
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  :config
  (company-mode 0)
  (irony-mode 0)
  (c-add-style "my-style" my-protobuf-style t))


(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred)
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (setq python-indent-offset 4
	python-environment-virtualenv '("virtualenv" "-p" "python3" "--system-site-packages" "--quiet")
	flycheck-python-flake8-executable
	"/home/tgolsson/anaconda3/envs/py38/bin/python3.8")
  (require 'dap-python)
  :init
  (use-package conda
    :commands conda-env-activate
    :init
    (setq conda-env-home-directory "/home/tgolsson/anaconda3"
          conda-anaconda-home "/home/tgolsson/anaconda3")
    :config
    (conda-env-activate "base")
    (conda-env-autoactivate-mode t))


  ;; (use-package jedi-core
  ;;   :hook (python-mode . jedi-mode)
  ;;   :init
  ;;   (use-package company-jedi
  ;;     :commands company-jedi)

  ;;   (setq jedi:server-command
  ;;         '("~/.emacs.d/.python-environments/default/bin/jediepcserver")
  ;;         jedi:use-shortcuts t)
  ;;   :config
  ;;   (jedi:setup))
  :config
  (flycheck-mode 1)
  (company-mode 1)
  ;; (make-local-variable 'company-backends)
  ;; (add-to-list 'company-backends '(company-jedi company-yasnippet)
  (flycheck-inline-mode 1)
  (modify-syntax-entry ?_  "_")


  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker))
(use-package pyvenv
  :after python-mode
  :init
  (setenv "WORKON_HOME" (expand-file-name "~/anaconda3/envs" ))

  :config
  (pyvenv-mode 1))
;; ;; CTAGS
;; (global-set-key (kbd "M-.")                  'ctags-search)

(use-package company-web)

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.html\\'" "\\.svelte\\'")
  :config
  (use-package company-web-html :commands web-mode)
  (use-package company-web-jade :commands web-mode)
  (use-package company-web-slim :commands web-mode)
  (use-package prettier-js :hook web-mode)
  (use-package add-node-modules-path :hook web-mode)
  :init
  (defun my-web-mode ()
    (make-local-variable 'yas-extra-modes)
    (add-to-list 'yas-extra-modes 'html-mode)
    (add-to-list 'yas-extra-modes 'php-mode)
    ;; make these variables local
    (make-local-variable 'web-mode-code-indent-offset)
    (make-local-variable 'web-mode-markup-indent-offset)
    (make-local-variable 'web-mode-css-indent-offset)

    (make-local-variable 'company-backends)
    (add-to-list 'company-backends '(company-web-html company-yasnippet))
    (define-key web-mode-map (kbd "C-<Space>") 'company-web-html)
    (company-mode 1)

    ;; set indentation, can set different indentation level for different code type
    (setq web-mode-code-indent-offset 4
          web-mode-css-indent-offset 4
          web-mode-markup-indent-offset 4
          web-mode-script-padding 4
          prettier-js-args '("--tab-width" "4"))

    (prettier-js-mode 1)
    (add-node-modules-path 1))

  (add-hook 'web-mode-hook 'my-web-mode))

(straight-use-package '(nxml-mode
  :type built-in
  :config
  (setq tab-width 4)))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :config
  (setq yaml-indent-offset 2))

(use-package dockerfile-mode
  :mode ("Dockerfile"))

(use-package glsl-mode
  :mode ("\\.glsl\\'"))
(use-package toml-mode)
(use-package jsonnet-mode)
(use-package graphviz-dot-mode)

(straight-use-package
 '( bazel-mode :type git :host github :repo "bazelbuild/emacs-bazel-mode"
))

;; typescript-mode
;; sass-mode
;; scss-mode
;; lua-mode

(measure-time
 "Starting server:"
 (when (and (fboundp 'server-running-p)
            (not (server-running-p)))
   (server-start)))

(measure-time "Loading custom:"
              ;; Keep emacs Custom-settings in separate file
              (load custom-file))

(setenv "SSH_ASKPASS" "git-gui --askpass")
(set-face-attribute 'mode-line nil :underline nil)
