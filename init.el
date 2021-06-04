(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; PRE-INIT
(setq gc-cons-threshold 500000000
      package-enable-at-startup nil
      load-prefer-newer t
      use-package-always-ensure t
      use-package-always-defer t
      package-list '(
                     ag
                     alert
                     anzu
                     bazel-mode
                     beacon
                     bind-key
                     bison-mode
                     cargo
                     cl-generic
                     clang-format
                     cmake-font-lock
                     cmake-mode
                     company
                     company-c-headers
                     company-emacs-eclim
                     company-emoji
                     company-glsl
                     company-go
                     company-irony
                     company-irony-c-headers
                     company-jedi
                     company-math
                     company-quickhelp
                     company-web
                     conda
                     copy-as-format
                     ;; dashboard
                     diminish
                     discover-my-major
                     docker
                     docker-tramp
                     dockerfile-mode
                     dumb-jump
                     ein
                     exec-path-from-shell
                     expand-region
                     flx-ido
                     flycheck
                     flycheck-clang-tidy
                     flycheck-golangci-lint
                     flycheck-inline
                     flycheck-irony
                     flycheck-pos-tip
                     flycheck-rust
                     flymake-gjshint
                     flymake-rust
                     flymake-sass
                     gif-screencast
                     git-commit
                     gitignore-mode
                     glsl-mode
                     go-eldoc
                     go-gopath
                     go-mode
                     go-projectile
                     golint
                     graphviz-dot-mode
                     guide-key
                     guide-key-tip
                     helm
                     helm-ag
                     helm-core
                     helm-descbinds
                     helm-describe-modes
                     helm-etags-plus
                     helm-flx
                     helm-flycheck
                     helm-flymake
                     helm-flyspell
                     helm-fuzzier
                     helm-ls-git
                     helm-projectile
                     helm-smex
                     helm-swoop
                     helm-w32-launcher
                     hl-block-mode
                     hlinum
                     ibuffer-projectile
                     irony
                     java-snippets
                     jedi-core
                     js-auto-format-mode
                     js2-highlight-vars
                     js2-mode
                     js2-refactor
                     jsonnet-mode
                     jtags
                     less-css-mode
                     lsp-typescript
                     lua-mode
                     magit
                     magit-filenotify
                     magit-popup
                     magit-todos
                     markdown-mode
                     markdown-mode+
                     markdown-preview-eww
                     markdown-toc
                     material-theme
                     math-symbol-lists
                     modern-cpp-font-lock
                     multiple-cursors
                     p4
                     pkg-info
                     popup
                     popwin
                     pos-tip
                     powerline
                     powershell
                     projectile
                     python-environment
                     rainbow-mode
                     request
                     request-deferred
                     rotate
                     rtags
                     rustic
                     sass-mode
                     scss-mode
                     seq
                     simple-httpd
                     skewer-mode
                     slack
                     smex
                     smooth-scrolling
                     spaceline
                     speed-type
                     take-off
                     tiny
                     toml-mode
                     typescript-mode
                     use-package
                     visual-fill-column
                     w3m
                     web-beautify
                     web-completion-data
                     web-mode
                     wgrep
                     with-editor
                     xcscope
                     yaml-mode
                     yapfify
                     yasnippet))

(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes"
                                 user-emacs-directory))

;; (require 'auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)



(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package benchmark-init
  :demand t
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(prefer-coding-system 'utf-8) ;; fixes some packages containing non-iso-latin characets

(use-package exec-path-from-shell
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

;; install the missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (progn
;;       (message "Installing: %s" package)
;;       (package-install package))))

(defun risky-local-variable-p (sym &optional _ignored) nil)

(defmacro measure-time (msg &rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s %.06f" ,msg (float-time (time-since time)))))

(defun to/append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(defun to/do-list-dir (thedir)
  "Iterates over all files in THEDIR and loads them"
  (if (file-accessible-directory-p thedir)
      (progn
        (dolist (file (directory-files thedir t "\.el$" nil))
          (load (file-name-sans-extension file) t t)))))

(defmacro to/easy-hook
    (hook prog &optional local append)
  `(add-hook ,hook (lambda () (interactive) ,prog) (when (boundp 'local) local)
             (when (boundp 'append) append)))

(defun to/my-require (symbol)
  "Requires a package and then prints that it was loaded"
  (require symbol))

(setq settings-dir (expand-file-name "settings" user-emacs-directory)
      modes-dir (expand-file-name "modes" user-emacs-directory)
      experiments-dir (expand-file-name "experiments" user-emacs-directory)
      packages-dir (expand-file-name "packages" user-emacs-directory)
      privates-dir (expand-file-name "private" user-emacs-directory)
      experiments-dir (expand-file-name "experiments" user-emacs-directory)
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path experiments-dir)
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path modes-dir)
(add-to-list 'load-path packages-dir)
(add-to-list 'load-path (expand-file-name "emacs-bazel-mode" packages-dir))

(measure-time "Loading all settings:"
              (to/my-require 'modeline-setup)
              (to/my-require 'appearance-setup)
              (to/my-require 'lsp-setup)
              (to/my-require 'basic-setup)
              (to/my-require 'input-setup)
              (to/my-require 'project-setup)
              (to/my-require 'defuns)
              (to/my-require 'modes-setup)
              (to/my-require 'magit-setup)
              (to/my-require 'company-setup)
              (to/my-require 'flycheck-setup)
              (to/my-require 'helm-setup)
              (to/my-require 'keybindings)
              (to/my-require 'single-line-hooks)

              (to/my-require 'diminish-setup))

;; Load modes
(measure-time "Loading all auxillaries:"
              (to/do-list-dir modes-dir)
              (to/do-list-dir privates-dir)
              (to/do-list-dir experiments-dir))

;; (to/my-require 'scimax-org-babel-python)
;; (to/my-require 'fira-code-data)
;; (to/my-require 'fira-code-mode)
;; (to/my-require 'darkroom-setup)

(measure-time "Starting server:"
              (when (and (fboundp 'server-running-p) (not (server-running-p)))
                (server-start)))

(measure-time "Loading custom:"
              ;; Keep emacs Custom-settings in separate file
              (load custom-file))

(to/easy-hook 'emacs-startup-hook (progn ) t)
(setenv "SSH_ASKPASS" "git-gui --askpass")
(set-face-attribute 'mode-line nil :underline nil)
