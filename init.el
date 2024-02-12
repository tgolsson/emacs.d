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
  (global-so-long-mode 1)

  (to/pushn load-path packages-dir experiments-dir)
  (push '(fullscreen . maximized) default-frame-alist)

  (set-fringe-mode 20)
  (setq-default custom-safe-themes t
                transient-mark-mode t
				tab-width 4)

  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (setq
   read-process-output-max (* 1024 1024)
   bidi-paragraph-direction 'left-to-right
   bidi-inhibit-bpa t
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

  (when window-system (set-frame-font "Cascadia Code 14"))
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
                               user-emacs-directory))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  )
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
  :disabled
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

;; (use-package all-the-icons)

(use-package doom-modeline
  :custom
  (doom-modeline-lsp t)
  (doom-modeline-lsp-icon t)
  :init
  (doom-modeline-mode 1)
  (set-face-attribute 'mode-line nil :underline nil)
  :config
  (set-face-attribute 'mode-line nil :underline nil)
  (when (not (memq window-system '(w32))) (nerd-icons-install-fonts t)))

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode)
;;   :config
;;   (defun to/set-emoji-font (frame)
;;     "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;     (if (eq system-type 'darwin)
;;         (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
;;       (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))
;;   (to/set-emoji-font nil)
;;   (add-hook 'after-make-frame-functions 'to/set-emoji-font))

;; (use-package font-lock
;;   :straight (:type built-in)
;;   :custom
;;   (font-lock-multiline t)
;;   :hook (((emacs-lisp-mode lua-mode) . font-lock-mode)))

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
  :custom (frame-background-mode 'light)
  :config
  (load-theme 'solarized-light t)
  (enable-theme 'solarized-light))

(defun dw/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . dw/org-mode-visual-fill))

(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General utility

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))


;;; General purpose code tools

(use-package dap-mode
  :commands dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (eglot-enable-dap-auto-configure nil)
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
  :bind (:map eglot-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra)))

(use-package dumb-jump)

(defun ts/eglot-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'eglot-capf completion-category-defaults))
        '(orderless))) ;; Configure orderless

(use-package yasnippet)
(yas-global-mode 1)

;; (use-package eglot-mode
;;   :custom
;;   (eglot-eslint-download-url "https://github.com/emacs-eglot/eglot-server-binaries/blob/master/dbaeumer.vscode-eslint-2.2.2.vsix?raw=true")
;;   :hook (((rust-mode go-mode) . eglot-mode)
;;          ((rust-mode) . eglot-lens-mode)
;; 		 (eglot-completion-mode . ts/eglot-mode-setup-completion)
;; 		 (eglot-mode . corfu-mode)
;; 		 (eglot-mode . (lambda ()
;; 							  (setq flycheck-local-checkers
;; 									'((eglot .
;; 										   ((next-checkers
;; 											 . (typos)))))))))

;;   :commands (eglot ls-deferred)
;;   :custom
;;   (eglot-completion-provider :none)
;;   :init
;;   (use-package eglot-treemacs :after eglot)
;;   (use-package eglot-ivy :after eglot)
;;   (use-package eglot-ui
;;     :commands eglot-ui-mode
;;     :bind-keymap ("C-c C-l" . eglot-command-map)
;;     :config
;;     (setq eglot-ui-sideline-enable t
;;           eglot-ui-flycheck-enable t
;;           eglot-ui-flycheck-list-position 'right
;;           eglot-ui-flycheck-live-reporting t)
;;     (setq eglot-go-env (make-hash-table :test 'equal))
;;     (puthash "GOPROXY" "https://proxy.golang.org,direct" eglot-go-env))

;;   :config
;;   (add-to-list 'eglot-file-watch-ignored-directories "[/\\\\]target\\'")
;;   (add-to-list 'eglot-file-watch-ignored-directories "[/\\\\]cargo\\'")
;;   (add-to-list 'eglot-file-watch-ignored-directories "[/\\\\]node_modules\\'")
;;   (add-to-list 'eglot-file-watch-ignored-directories "[/\\\\]\\.venv\\'")
;;   (add-to-list 'eglot-file-watch-ignored-directories "[/\\\\]third-party\\'")
;;   (add-to-list 'eglot-file-watch-ignored-directories "[/\\\\]bazel-bin\\'")
;;   (add-to-list 'eglot-file-watch-ignored-directories "[/\\\\]bazel-out\\'")
;;   (add-to-list 'eglot-file-watch-ignored-directories "[/\\\\]bazel-src\\'")
;;   (add-to-list 'eglot-file-watch-ignored-directories "[/\\\\]bazel-testlogs\\'")


;;   (setq eglot-disabled-clients '(rls)
;;         eglot-headerline-breadcrumb-enable nil
;;         eglot-headerline-breadcrumb-icons-enable nil
;;         eglot-prefer-flymake nil
;; 		eglot-idle-delay 0.500
;;         eglot-ui-doc-use-childframe t)
;;   (eglot-enable-which-key-integration t))

(use-package eglot
  :ensure t
  :hook ((( rust-mode go-mode python-mode web-mode) . eglot-ensure))
  :custom
  (eglot-report-progress t)
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t))

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

(use-package delight)

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


(use-package cascadia-code
  :straight (:type built-in)
  :hook (prog-mode . cascadia-code-mode))

(use-package projectile
  :diminish t
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


(defun wsl-copy-region-to-clipboard (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(define-key global-map (kbd "M-o") 'wsl-copy-region-to-clipboard)

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

(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp.")

(defun enable-minor-mode-based-on-extension ()
  "Check file name against `auto-minor-mode-alist' to enable minor modes
the checking happens for all pairs in auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name (file-name-sans-versions buffer-file-name))
          (remote-id (file-remote-p buffer-file-name))
          (case-fold-search auto-mode-case-fold)
          (alist auto-minor-mode-alist))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook #'enable-minor-mode-based-on-extension)
(add-to-list 'auto-minor-mode-alist '("\\.svelte\\'" . eglot-mode))


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
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
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

  (irony-mode 1)
  (electric-pair-mode 1)
  (add-hook 'before-save-hook 'clang-format-buffer t t))

;;
;; irony-mode
;;
(use-package irony
  :commands irony-mode
  :hook cc-mode
  :defer t
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

(use-package asm-mode :mode (("\\.tmpli\\'" . asm-mode)))

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
                    (lambda ()
                       (make-local-variable 'completion-at-point-functions)
                       (setq completion-at-point-functions '(elisp-completion-at-point comint--complete-file-name-data)
                             comint-completion-addsuffix nil)

                       (eldoc-mode 1)
                       (add-hook 'after-save-hook 'byte-compile-current-buffer nil t))))

(use-package abbrev-mode
  :straight (:type built-in)
  :hook (lua-mode lisp-mode))

(use-package go-mode
  :custom
  (eglot-go-directory-filters ["-bazel-src" "-bazel-bin" "-bazel-out" "-bazel-testlogs"])
  :mode "\\.go\\'"
  :init
  ;; go-projectile
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
    (eglot-ensure)
    (flycheck-golangci-lint-setup)
    (flycheck-pos-tip-mode 1)
    (add-hook 'before-save-hook 'gofmt-before-save nil t))
  (add-hook 'go-mode-hook 'to/my-go-mode))

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . hs-minor-mode)
		 (rust-mode . rust-enable-format-on-save)
         (rust-mode . (lambda ()
						(eglot-ensure)
						(corfu-mode 1)
                        (flycheck-pos-tip-mode 0)
                        ;; (flycheck-inline-mode 0)
                        (set (make-local-variable 'compile-command) "cargo run")
                        (add-hook 'before-save-hook #'eglot-format-buffer t t))))
  :init
  (use-package cargo
    :hook (rust-mode . cargo-minor-mode))

  ;; (setq eglot-rust-server 'rust-analyzer
  ;;       eglot-rust-analyzer-server-display-inlay-hints t
  ;;       eglot-rust-analyzer-display-parameter-hints t
  ;;       eglot-rust-analyzer-display-chaining-hints t)

  )

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook ((typescript-mode . hs-minor-mode)
		 (typescript-mode . (lambda ()
							  (eglot-ensure)
							  (prettier-js-mode 1)))))

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :hook (protobuf-mode . (lambda () (c-add-style "my-style" my-protobuf-style t)))
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil))))

(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . eglot-ensure)
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

  (use-package python-black
    :demand t
    :after python
    :hook (python-mode . python-black-on-save-mode))

  (use-package python-isort
    :demand t
    :after python
    :hook (python-mode . python-isort-on-save-mode))

  :config
  (require 'dap-python)
  (modify-syntax-entry ?_  "_")
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker))

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.html\\'" "\\.svelte\\'")
  :hook (web-mode . (lambda ()
					  (eglot-ensure 1)))
  :init
  (use-package prettier-js
      :init
      (setq prettier-js-args '("--tab-width" "4" "--use-tabs" "true")))

  (use-package add-node-modules-path
    :hook web-mode)
  (setq indent-tabs-mode t
		web-mode-code-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-markup-indent-offset 4
        web-mode-script-padding 4))

;; INFRA AND TOOLS
(use-package dockerfile-mode :mode ("Dockerfile"))
(use-package bazel)

;; RENDERING
(use-package glsl-mode :mode ("\\.glsl\\'"))

;; MARKUP LANGUAGES
(use-package toml-mode)
(use-package jsonnet-mode
  :config
  :hook (jsonnet-mode . (lambda ()
		  (progn
		  (indent-tabs-mode -1)
		  (setq indent-tabs-mode nil
				tab-width 2)))))
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
  (use-package markdown-toc
    :commands (markdown-toc-generate-toc markdown-toc-refresh-toc)))

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-info corfu-history))

  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.3)         ;; After 0.0 seconds
  (corfu-auto-prefix 3)          ;; And a single character
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-echo-documentation t)   ;; Show documentation in the echo area
  (corfu-popupinfo-delay 0)
  :hook ((emacs-lisp-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
		 (corfu-mode . corfu-popupinfo-mode))

  :init

  (use-package kind-icon
	:after corfu
	:custom
	(kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly

	:config
	(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


  :bind (:map corfu-map ("M-d" . corfu-popupinfo--toggle))

  )

(use-package copilot
  :hook ((prog-mode . copilot-mode))
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :bind (:map copilot-mode-map (("C-<tab>" . copilot-accept-completion))))

;; typescript-mode
;; sass-mode
;; scss-mode

(use-package server
  :demand t
  :config
  (defun server-ensure-safe-dir (dir) "Noop" t)
  (unless (file-exists-p server-socket-dir)
	(make-directory server-socket-dir))
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
;;  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  ; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-."))

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

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Arial" :height 180 :weight thin))))
 '(fixed-pitch ((t ( :family "Cascadia Code" :height 160)))))

(use-package wgrep
  :demand t)
(put 'scroll-left 'disabled nil)

(flycheck-define-checker typos
  "A typo checker using typos-cli."
  :command ("typos" "--format" "brief" source)
  :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes (text-mode prog-mode markdown-mode rust-mode toml-mode python-mode))

(add-to-list 'flycheck-checkers 'typos)
