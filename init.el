;;; PRE-INIT
;;; To ensure setup is quick
(setq gc-cons-threshold 500000000
      load-prefer-newer t
      package-enable-at-startup nil)

(defun to/report-startup-time () ""
       (message "*** Emacs loaded in %s with %d garbage collections."
                (format "%.2f seconds"
                        (float-time
                         (time-subtract after-init-time before-init-time)))
                gcs-done))

(add-hook 'emacs-startup-hook 'to/report-startup-time)

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

;;; Init bootstrap
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

;;; In order to still use `package-list-packages' to find new toys.
(require 'use-package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;;; Use utf-8 everywhere it makes sense.
(define-coding-system-alias 'cp65001 'utf-8)
(prefer-coding-system 'utf-8) ;; fixes some packages containing non-iso-latin characets
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(eval-when-compile
  (require 'cl))

(defmacro to/pushn (list &rest vals)
  (let ((forms (mapcar (lambda (var) `(push ,var ,list)) vals)))
    `(progn ,@forms)))

(defmacro as-local-company-backends (&rest values)
  `(progn
     (make-local-variable 'company-backends)
     (setq company-backends ',values)))

(defmacro with-local-company-backends(&rest values)
  `(progn
     (make-local-variable 'company-backends)
     (add-to-list 'company-backends ',values)))

(defmacro to/set-safe (name value) `(when (boundp ',name) (setq ,name
                                                                ,value)))

(defmacro to/disable (name) `(when (fboundp ',name) (,name -1)))

(defmacro measure-time (msg &rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time))) ,@body (message "%s %.06f" ,msg (float-time (time-since time)))))

;; generic setq
(setq experiments-dir (expand-file-name "experiments" user-emacs-directory)
      packages-dir (expand-file-name "packages" user-emacs-directory)
      custom-file (expand-file-name "custom.el" user-emacs-directory))


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

(defun risky-local-variable-p (sym &optional _ignored) "Noop" nil)

(use-package emacs
  :demand t
  :init
  (to/pushn load-path packages-dir experiments-dir)
  (push '(fullscreen . maximized) default-frame-alist)

  (set-fringe-mode 20)
  (setq-default custom-safe-themes t
                transient-mark-mode t)

  (setq
   apropos-do-all t
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   backup-directory-alist `((".*" . ,temporary-file-directory))
   bookmark-default-file  (concat user-emacs-directory "bookmarks.em")
   bookmark-save-flag 1
   compilation-scroll-output 'first-error
   confirm-kill-emacs 'y-or-n-p
   cursor-type t
   delete-selection-mode t
   echo-keystrokes 0.1
   ediff-diff-options "-w"
   ediff-split-window-function 'split-window-horizontally
   ediff-window-setup-function 'ediff-setup-windows-plain
   fill-column 80
   frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))
   history-length 1000
   indent-tabs-mode nil
   inhibit-splash-screen t
   inhibit-startup-message t
   large-file-warning-threshold 100000000
   locale-coding-system 'utf-8
   mouse-yank-at-point t
   require-final-newline t
   ring-bell-function 'ignore
   save-interprogram-paste-before-kill t
   scroll-conservatively 100000
   scroll-margin 0
   scroll-preserve-screen-position 0
   user-full-name "Tom Solberg"
   user-mail-address "me@sbg.dev"
   vc-make-backup-files t
   x-select-enable-clipboard t
   x-select-enable-primary t)

  (when window-system (set-frame-font "Cascadia Code 10"))
  (fset 'yes-or-no-p 'y-or-n-p)
  (mapc 'frame-set-background-mode (frame-list))
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (setenv "SSH_ASKPASS" "git-gui --askpass")

  (transient-mark-mode 1)
  (make-variable-buffer-local 'transient-mark-mode)
  (put 'transient-mark-mode 'permanent-local t)


  (to/disable tool-bar-mode)
  (to/disable scroll-bar-mode)
  (to/disable blink-cursor-mode)
  (to/disable menu-bar-mode)
  (to/disable horizontal-scroll-bar-mode)

  (load-file (expand-file-name (if (eq system-type 'windows-nt)
                                   "windows-setup.el"
                                 "unix-setup.el")
                               user-emacs-directory)))
;; Requires early setup
(use-package no-littering)

(use-package benchmark-init
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package comint
  :straight (:type built-in)
  :init
  (defun to/preoutput-turn-buffer-read-only (text)
    (propertize text 'read-only t))
  :custom
  (comint-prompt-readonly t)
  (comint-buffer-maximum-size 20000 "Increase comint buffer size.")
  (comint-prompt-read-only t "Make the prompt read only.")
  :config
  (add-hook 'comint-preoutput-filter-functions
            'to/preoutput-turn-buffer-read-only))

(use-package desktop
  :hook (after-init-hook . desktop-read)
  :init (setq desktop-restore-eager 8
              desktop-auto-save-timeout 120)
  (desktop-save-mode t))

(use-package exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :init (setq exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))

(use-package savehist
  :init (savehist-mode +1)
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60))

(use-package recentf
  :bind (("<f6>" . counsel-recentf))
  :custom
  (recentf-max-saved-items 2000)
  :init
  (recentf-mode +1)
  :config
  (to/append-to-list 'recentf-exclude '("/elpa/"
                                        "company-statistics-cache.el"
                                        "bookmarks.em" ".mc-lists.el"
                                        "custom.el"
                                        no-littering-etc-directory
                                        no-littering-var-directory)))

(use-package saveplace
  :init (setq save-place-file (concat user-emacs-directory "places"))
  :config (save-place-mode))

(use-package uniquify
  :straight (:type built-in)
  :config (setq uniquify-buffer-name-style 'forward))

;;; Visuals

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (set-face-attribute 'mode-line nil :underline nil)
  :config
  (set-face-attribute 'mode-line nil :underline nil)
  (when (not (memq window-system '(w32))) (all-the-icons-install-fonts t)))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (defun to/set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (if (eq system-type 'darwin)
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
      (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))
  (to/set-emoji-font nil)
  (add-hook 'after-make-frame-functions 'to/set-emoji-font))

(use-package font-lock
  :straight (:type built-in)
  :custom
  (font-lock-multiline t)
  :hook (((emacs-lisp-mode lua-mode) . font-lock-mode)))

(use-package global-display-line-numbers-mode
  :straight (:type built-in)
  :commands global-display-line-numbers-mode
  :custom
  (display-line-numbers-major-tick 50)
  (display-line-numbers-minor-tick 10)
  :init
  (global-display-line-numbers-mode 1)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(use-package hl-line
  :demand t
  :init (global-hl-line-mode +1)
  :config
  (add-hook 'org-mode-hook (lambda ()  (setq-local global-hl-line-mode nil))))

(use-package paren
  :commands show-paren-mode
  :init
  (show-paren-mode +1))

(use-package rainbow-delimiters)
(use-package rainbow-mode :config (add-hook 'prog-mode-hook #'rainbow-mode))
(use-package rotate)
(use-package smooth-scrolling)

(use-package solarized-theme
  :demand t
  :custom (frame-background-mode 'dark)
  :config
  (load-theme 'solarized-dark t)
  (enable-theme 'solarized-dark))

(defun dw/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . dw/org-mode-visual-fill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

;;; Searching and navigation

(use-package anzu)

;;; General purpose code tools

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
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra)))

(use-package dumb-jump)

(use-package lsp-mode
  :hook (((rust-mode go-mode) . lsp-mode)
         ((rust-mode) . lsp-lens-mode))
  :commands (lsp ls-deferred)
  :init
  (use-package lsp-treemacs :after lsp)
  (use-package lsp-ivy :after lsp)
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

  :config
  (setq lsp-disabled-clients '(rls)
        lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-prefer-flymake nil
        lsp-prefer-capf t
        lsp-ui-doc-use-childframe nil)
  (lsp-enable-which-key-integration t))

(use-package yasnippet
  :diminish  t
  :commands (yas-global-mode)
  :init (yas-global-mode))

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

(use-package editorconfig)

;;
;; General input modes
;;
(global-subword-mode +1)

(use-package elec-pair
  :commands elec-pair-mode
  :hook (prog-mode . electric-pair-mode)
  :demand t
  :config (electric-pair-mode +1))

(use-package multiple-cursors
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

(use-package which-key
  :init (which-key-mode 1)
  :config (setq which-key-idle-delay 0.3))

(use-package autoinsert
  :custom
  (auto-insert-directory (expand-file-name "templates" user-emacs-directory))
  (auto-insert-query nil)
  :init
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  (define-auto-insert "\\.el$" ["default-lisp.el" to/autoinsert-yas-expand])
  (define-auto-insert "\\.py$" ["default-python.py" to/autoinsert-yas-expand])
  (define-auto-insert "\\.rs$" ["default-rust.rs" to/autoinsert-yas-expand])
  (define-auto-insert "/sprints/" ["sprint.org" to/autoinsert-yas-expand])
  (define-auto-insert "test_.*.py" ["test.py" to/autoinsert-yas-expand])
  ;; autoinsert C/C++ header
  (define-auto-insert (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "") ["header.h" to/autoinsert-yas-expand])
  (define-auto-insert (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "") ["header.cpp" to/autoinsert-yas-expand]))

;; (use-package hideshow
;;   :config
;;   (define-key hs-minor-mode-map (kbd "C-c h") (lookup-key hs-minor-mode-map (kbd "C-c @")))
;;   (define-key hs-minor-mode-map (kbd "C-c @") nil))


;; (use-package expand-region
;;   :bind ("C-@" . er/expand-region))

(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Aile"
                    :height 100
                    :weight 'light)

(set-face-attribute 'fixed-pitch nil
                    :font "Cascadia Code"
                    :height 100)

(use-package cascadia-code
  :straight (:type built-in)
  :hook (prog-mode . cascadia-code-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :defer 10
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (use-package ibuffer-projectile
    :hook (ibuffer . (lambda ()
                       (ibuffer-projectile-set-filter-groups)
                       (unless (eq ibuffer-sorting-mode 'alphabetic)
                         (ibuffer-do-sort-by-alphabetic)))))

  ;; (use-package counsel-projectile
  ;;   :after projectile
  ;;   :config (counsel-projectile-mode))

  :config
  (setq projectile-switch-project-action 'magit-status
        projectile-globally-ignored-directories (append '("*__pycache__*" "*.egg-info") projectile-globally-ignored-directories)
        projectile-globally-ignored-file-suffixes (append '(".pyc") projectile-globally-ignored-file-suffixes)
        projectile-indexing-method 'alien
        projectile-enable-caching 't
        projectile-git-command "fd . -0"
        projectile-git-submodule-command "git submodule --quiet foreach 'echo $path'")
  (projectile-mode +1)
  (projectile-global-mode 1))


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
  (delete-trailing-whitespace))

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



;;
;; magit
;;
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status))
  :bind (:map magit-status-mode-map ( "q" . magit-quit-session))
  :init
  (use-package magit-filenotify
    :if (not (memq window-system '(w32)))
    :hook (magit-status-mode . magit-filenotify-mode))
  ;; (use-package magit-todos
  ;;   :hook (magit-status-mode . magit-todos-mode))
  :config
  (setq magit-todos-nice nil)
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

(use-package flycheck
  :hook ((cc-mode python-mode rust-mode go-mode) . flycheck-mode)
  :diminish t
  :commands flycheck-mode
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-disabled-checkers '(python-pylint))
  :init
  (use-package flycheck-clang-tidy
    :hook (cc-mode . flycheck-clang-tidy-setup)
    :after flycheck)
  (use-package flycheck-pos-tip
    :hook (flycheck-mode . flycheck-pos-tip-mode)
    :config (custom-set-variables '(flycheck-display-errors-function
                                    #'flycheck-pos-tip-error-messages))
    (make-variable-buffer-local 'flycheck-idle-change-delay))
  :config
  (add-hook 'flycheck-after-syntax-check-hook 'to/adjust-flycheck-automatic-syntax-eagerness))

;; (use-package ivy
;;   :diminish
;;   :bind (("M-s" . swiper)
;;          :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done)
;;          ("C-l" . ivy-alt-done)
;;          :map ivy-switch-buffer-map
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (setq ivy-use-virtual-buffers t
;;         ivy-initial-inputs-alist '((counsel-minor . "^+")
;;                                    (counsel-package . "^+")
;;                                    (counsel-org-capture . "^")
;;                                    (counsel-M-x . "")
;;                                    (counsel-describe-symbol . "^")
;;                                    (org-refile . "^")
;;                                    (org-agenda-refile . "^")
;;                                    (org-capture-refile . "^")
;;                                    (Man-completion-table . "^")
;;                                    (woman . "^")))
;;   (ivy-mode 1)
;;   :init
;;   ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))


;;   (global-set-key (kbd "C-s") 'swiper-isearch)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "M-y") 'counsel-yank-pop)
;;   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;   (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;   (global-set-key (kbd "<f2> j") 'counsel-set-variable)
;;   (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;;   (global-set-key (kbd "C-c v") 'ivy-push-view)
;;   (global-set-key (kbd "C-c V") 'ivy-pop-view))

;; (use-package ivy-rich
;;   :after ivy
;;   :init
;;   (ivy-rich-mode 1))

;; (use-package counsel
;;   :bind (("C-M-j" . 'counsel-switch-buffer)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history))
;;   :custom
;;   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
;;   :config
;;   (counsel-mode 1))

;; (use-package ivy-prescient
;;   :after counsel
;;   :custom
;;   (ivy-prescient-enable-filtering nil)
;;   :config
;;   ;; Uncomment the following line to have sorting remembered across sessions!
;;   (prescient-persist-mode 1)
;;   (ivy-prescient-mode 1))

;; Make commit-lines 72 lines max
(add-hook 'git-commit-mode-hook (lambda () (interactive "") (set-fill-column 72)))

;;
;; Diminish
;;
(use-package diminish
  :demand t
  :defer 10
  :config)

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
  :hook (cc-mode .  (lambda () (with-local-company-backends company-irony company-irony-c-headers company-yasnippet)))
  :init
  (setq-default c-default-style "linux"
                c-basic-offset 4)
  :config
  (c-set-offset 'inline-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'topmost-intro 0)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'member-init-intro 0)

  (irony-mode 1)
  (company-irony 1)
  (electric-pair-mode 1)
  (add-hook 'before-save-hook 'clang-format-buffer t t))

;;
;; irony-mode
;;
(use-package irony
  :commands irony-mode
  :hook cc-mode
  :defer t
  :init
  (use-package company-irony-c-headers
    :commands company-irony-c-headers)
  (use-package company-irony
    :commands company-irony)
  :config
  (irony-cdb-autosetup-compile-options))

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
  :hook (cmake-mode . (lambda () (as-local-company-backends company-cmake company-yasnippet)))
  :init
  (use-package company-cmake
    :straight (:type git :host github :repo "purcell/company-cmake")
    :hook cmake-mode)
  (use-package cmake-font-lock
    :commands cmake-font-lock-activate
    :hook (cmake-mode . cmake-font-lock-activate)))

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p
              (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-ignore-case t
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package lisp-mode
  :straight (:type built-in)
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map ("\r"
                                   . reindent-then-newline-and-indent))
  :config (add-hook 'emacs-lisp-mode-hook
                    '(lambda ()
                       (make-local-variable 'completion-at-point-functions)
                       (setq completion-at-point-functions '(elisp-completion-at-point comint--complete-file-name-data)
                             comint-completion-addsuffix nil)

                       (eldoc-mode 1)
                       (flyspell-prog-mode)
                       (add-hook 'after-save-hook 'byte-compile-current-buffer nil t))))

(use-package abbrev-mode
  :straight (:type built-in)
  :hook (lua-mode lisp-mode))

(use-package lua-mode
  :mode "\\.lua\\'"
  :init
  (use-package company-lua
    :commands company-lua
    :hook (lua-mode . (lambda () (with-local-company-backends (company-lua company-yasnippet)))))
  :config
  (auto-fill-mode 0))

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
    (go-projectile-tools-add-path)
    (as-local-company-backends company-capf)
    (lsp)
    (flycheck-golangci-lint-setup)
    (flycheck-pos-tip-mode 1)
    (flycheck-add-next-checker 'lsp 'golangci-lint)
    (add-hook 'before-save-hook 'gofmt-before-save nil t))
  (add-hook 'go-mode-hook 'to/my-go-mode))

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . lsp-rust-analyzer-inlay-hints-mode)
         (rust-mode . hs-minor-mode)
         (rust-mode . (lambda ()
                        (lsp 1)
                        (flycheck-pos-tip-mode 0)
                        (flycheck-inline-mode 0)
                        (set (make-local-variable 'compile-command) "cargo run")
                        (add-hook 'before-save-hook #'lsp-format-buffer t t))))
  :init
  (use-package cargo
    :hook (rust-mode . cargo-minor-mode))

  (setq lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-chaining-hints t))

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :hook (protobuf-mode . (lambda ()
                           (company-mode 0)
                           (irony-mode 0)
                           (c-add-style "my-style" my-protobuf-style t)))
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil))))

(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred)
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (setq python-indent-offset 4
        python-environment-virtualenv '("virtualenv" "-p" "python3" "--system-site-packages" "--quiet")
        flycheck-python-flake8-executable "/home/tgolsson/anaconda3/envs/py38/bin/python3.8")

  :init
  (use-package pyvenv
    :after python-mode
    :init
    (setenv "WORKON_HOME" (expand-file-name "~/anaconda3/envs" ))
    :config
    (pyvenv-mode 1))

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
  (require 'dap-python)
  ;; (make-local-variable 'company-backends)
  ;; (add-to-list 'company-backends '(company-jedi company-yasnippet)
  ;; (flycheck-inline-mode 1)
  (modify-syntax-entry ?_  "_")
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker))

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.html\\'" "\\.svelte\\'")
  :hook (web-mode . (lambda ()
                      (make-local-variable 'yas-extra-modes)
                      (add-to-list 'yas-extra-modes 'html-mode)
                      (add-to-list 'yas-extra-modes 'php-mode)
                      (with-local-company-backends (company-web-html company-yasnippet))))
  :init
  (use-package company-web)
  ;; TODO: These are actually not use-package things.
  ;; (use-package company-web-html :commands web-mode)
  ;; (use-package company-web-jade :commands web-mode)
  ;; (use-package company-web-slim :commands web-mode)
  (use-package prettier-js
    :hook (web-mode . prettier-js-mode)
    :init
    (setq prettier-js-args '("--tab-width" "4")))
  (use-package add-node-modules-path
    :hook web-mode)
  (setq web-mode-code-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-markup-indent-offset 4
        web-mode-script-padding 4)
  :config
  (define-key web-mode-map (kbd "C-<Space>") 'company-web-html))

;; INFRA AND TOOLS
(use-package dockerfile-mode
  :mode ("Dockerfile"))
(use-package bazel)

;; RENDERING
(use-package glsl-mode :mode ("\\.glsl\\'"))

;; MARKUP LANGUAGES
(use-package nxml-mode :straight (:type built-in))
(use-package toml-mode)
(use-package jsonnet-mode)
(use-package yaml-mode :mode ("\\.yaml$" "\\.yml$") :custom  (yaml-indent-offset 2))
(use-package graphviz-dot-mode)

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
    :commands company-emoji
    :hook (markdown-mode . (lambda () (with-local-company-backends company-emoji))))
  (use-package markdown-toc
    :commands (markdown-toc-generate-toc markdown-toc-refresh-toc)))

;;; COMPLETION

(use-package company
  :after lsp-mode
  :hook ((web-mode python-mode lua-mode cmake-mode go-mode cc-mode) . company-mode)
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
  :config
  (define-key company-active-map (kbd "TAB") 'tab-indent-or-complete)
  (define-key company-active-map (kbd "<tab>") 'tab-indent-or-complete))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.0)         ;; After 0.0 seconds
  (corfu-auto-prefix 1)          ;; And a single character
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-echo-documentation t)   ;; Show documentation in the echo area

  :hook ((emacs-lisp-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :init
  ;; (corfu-global-mode)
  )

;; typescript-mode
;; sass-mode
;; scss-mode

(use-package server
  :ensure t
  :demand t
  :config
  (defun server-ensure-safe-dir (dir) "Noop" t)
  (unless (server-running-p)
    (server-start)))

;; Keep emacs Custom-settings in separate file
(measure-time "Loading custom:" (load custom-file))

;;; General fun stuff
(use-package copy-as-format)
(use-package gif-screencast)
(use-package speed-type)

(global-set-key (kbd "C-x C-b")              'ibuffer)
(global-set-key (kbd "M-z")                  'zap-up-to-char)

(global-set-key (kbd "C-s")                  'isearch-forward-regexp)
(global-set-key (kbd "C-r")                  'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")                'isearch-forward)
(global-set-key (kbd "C-M-r")                'isearch-backward)
(global-set-key (kbd "M-j")  (lambda () (interactive) (join-line -1)))

(global-set-key (kbd "C-x C-r")              'rename-current-buffer-file)

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

;; programming mode
(define-key prog-mode-map (kbd "M-W") 'yank-arg)

(bind-key "M-Q" 'delete-trailing-whitespace)



;;; VERTICO TEST
(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(defvar consult--fd-command nil)
(defun consult--fd-builder (input)
  (unless consult--fd-command
    (setq consult--fd-command
          (if (eq 0 (call-process-shell-command "fdfind"))
              "fdfind"
            "fd")))
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler
                                      arg 'extended)))
    (when re
      (list :command (append
                      (list consult--fd-command
                            "--color=never" "--full-path"
                            (consult--join-regexps re 'extended))
                      opts)
            :highlight hl))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'word)))

(use-package consult
  :straight t
  :demand t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-fd)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line-symbol-at-point)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
	 ("<f6>" . consult-recent-file)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-.")))

(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package embark
  :straight t
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :straight '(embark-consult :host github
                             :repo "oantolin/embark"
                             :files ("embark-consult.el"))
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

(use-package vertico
  :straight '(vertico :host github
                      :repo "minad/vertico"
                      :branch "main")
  :bind (:map vertico-map
              :map minibuffer-local-map
              ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Arial" :height 180 :weight thin))))
 '(fixed-pitch ((t ( :family "Cascadia Code" :height 160)))))

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

;; Make sure Straight pulls Org from Guix
(straight-use-package '(org :type built-in))

(use-package org
  :defer t
  :hook (org-mode . dw/org-mode-setup)
  :init
  (use-package ox-hugo
    :custom
    (org-hugo-default-static-subdirectory-for-externals "org")
    :config
    (push "py" org-hugo-external-file-extensions-allowed-for-copying))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
	org-src-preserve-indentation t)

  (setq org-modules
	'(org-crypt
          org-habit))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ledger . t)
     (dot . t)
     (python . t)))

  
  (defun cpb/convert-attachment-to-file ()
  "Convert [[attachment:..]] to [[file:..][file:..]]"
  (interactive)
  (let ((elem (org-element-context)))
    (if (eq (car elem) 'link)
        (let ((type (org-element-property :type elem)))
          ;; only translate attachment type links
          (when (string= type "attachment")
            ;; translate attachment path to relative filename using org-attach API
            ;; 2020-11-15: org-attach-export-link was removed, so had to rewrite
            (let* ((link-end (org-element-property :end elem))
                   (link-begin (org-element-property :begin elem))
                   ;; :path is everything after attachment:
                   (file (org-element-property :path elem))
                   ;; expand that to the full filename
                   (fullpath (org-attach-expand file))
                   ;; then make it relative to the directory of this org file
                   (current-dir (file-name-directory (or default-directory
                                                         buffer-file-name)))
                   (relpath (file-relative-name fullpath current-dir)))
              ;; delete the existing link
              (delete-region link-begin link-end)
              ;; replace with file: link and file: description
              (insert (format "[[file:%s][file:%s]]" relpath relpath))))))))
  (defun to/org-confirm-babel-evaluate (lang body)
    (not (member lang '("python" "ditaa"))))  ;don't ask for ditaa
  (setq org-confirm-babel-evaluate #'to/org-confirm-babel-evaluate)
  (use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 100)
  (dolist (face '((org-level-1 . 120)
                  (org-level-2 . 110)
                  (org-level-3 . 105)
                  (org-level-4 . 100)
                  (org-level-5 . 110)
                  (org-level-6 . 110)
                  (org-level-7 . 110)
                  (org-level-8 . 110)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

  ;; Make sure org-indent face is available
  (require 'org-indent)

  (set-face-attribute 'variable-pitch nil :height 100)
  (set-face-attribute 'fixed-pitch nil :height 100)
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))
