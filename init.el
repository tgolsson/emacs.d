(setq package-enable-at-startup nil)
(package-initialize)
;; GENERAL SETTINGS
(require 'package) ;; You might already have this line
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(setq use-package-always-ensure t)
;; (setq load-prefer-newer t)

(setq package-list '(
                     alert ; needs review
                     android-mode ; needs review
                     anzu ; needs review
                     arduino-mode ; needs review
                     auctex ; needs review
                     auctex-latexmk ; needs review
                     beacon ; needs review
                     bind-key ; needs review
                     bison-mode ; needs review
                     cargo ; needs review
                     cl-generic ; needs review
                     clang-format ; needs review
                     cmake-font-lock ; needs review
                     cmake-mode ; needs review
                     company ; needs review
                     company-arduino ; needs review
                     company-auctex ; needs review
                     company-c-headers ; needs review
                     company-glsl ; needs review
                     company-irony ; needs review
                     company-irony-c-headers ; needs review
                     company-jedi ; needs review
                     company-lua ; needs review
                     company-math ; needs review
                     company-racer ; needs review
                     company-statistics ; needs review
                     company-web ; needs review
                     conda ; needs review
                     copy-as-format ; needs review
                     ctags-update ; needs review
                     darkroom ; needs review
                     dashboard ; needs review
                     demo-it ; needs review
                     diminish ; needs review
                     discover-my-major ; needs review
                     docker ; needs review
                     docker-tramp ; needs review
                     dockerfile-mode ; needs review
                     draft-mode ; needs review
                     dumb-jump ; needs review
                     elfeed ; needs review
                     elfeed-goodies ; needs review
                     elfeed-org ; needs review
                     epresent ; needs review
                     exec-path-from-shell ; needs review
                     expand-region ; needs review
                     fancy-battery ; needs review
                     fasd ; needs review
                     flx-ido ; needs review
                     flycheck ; needs review
                     flycheck-clang-tidy ; needs review
                     flycheck-inline ; needs review
                     flycheck-irony ; needs review
                     flycheck-pos-tip ; needs review
                     flycheck-rust ; needs review
                     flymake-gjshint ; needs review
                     flymake-sass ; needs review
                     gif-screencast ; needs review
                     git-commit ; needs review
                     gitignore-mode ; needs review
                     glsl-mode ; needs review
                     gradle-mode ; needs review
                     graphviz-dot-mode ; needs review
                     gscholar-bibtex ; needs review
                     guide-key ; needs review
                     guide-key-tip ; needs review
                     guru-mode ; needs review
                     helm ; needs review
                     helm-ag ; needs review
                     helm-bibtex ; needs review
                     helm-core ; needs review
                     helm-descbinds ; needs review
                     helm-describe-modes ; needs review
                     helm-etags-plus ; needs review
                     helm-flx ; needs review
                     helm-flycheck ; needs review
                     helm-flymake ; needs review
                     helm-flyspell ; needs review
                     helm-fuzzier ; needs review
                     helm-ls-git ; needs review
                     helm-mu ; needs review
                     helm-projectile ; needs review
                     helm-smex ; needs review
                     helm-swoop ; needs review
                     helm-w32-launcher ; needs review
                     hide-lines ; needs review
                     hlinum ; needs review
                     html-script-src ; needs review
                     htmlize ; needs review
                     ibuffer-projectile ; needs review
                     irony ; needs review
                     java-snippets ; needs review
                     jdee ; needs review
                     jedi-core ; needs review
                     js-auto-format-mode ; needs review
                     js2-highlight-vars ; needs review
                     js2-mode ; needs review
                     js2-refactor ; needs review
                     jtags ; needs review
                     latex-extra ; needs review
                     less-css-mode ; needs review
                     lua-mode ; needs review
                     magit ; needs review
                     magit-filenotify ; needs review
                     magit-popup ; needs review
                     magit-todos ; needs review
                     markdown-mode ; needs review
                     markdown-mode+ ; needs review
                     markdown-preview-eww ; needs review
                     markdown-toc ; needs review
                     math-symbol-lists ; needs review
                     matlab-mode ; needs review
                     modern-cpp-font-lock ; needs review
                     multiple-cursors ; needs review
                     ob-browser ; needs review
                     ob-http ; needs review
                     org ; needs review
                     org-beautify-theme ; needs review
                     org-bullets ; needs review
                     org-mime ; needs review
                     p4 ; needs review
                     php-mode ; needs review
                     pkg-info ; needs review
                     popup ; needs review
                     popwin ; needs review
                     pos-tip ; needs review
                     powerline ; needs review
                     powershell ; needs review
                     projectile ; needs review
                     python-django ; needs review
                     python-environment ; needs review
                     racer ; needs review
                     rainbow-mode ; needs review
                     request ; needs review
                     request-deferred ; needs review
                     rotate ; needs review
                     rtags ; needs review
                     rust-mode ; needs review
                     sass-mode ; needs review
                     scss-mode ; needs review
                     seq ; needs review
                     simple-httpd ; needs review
                     skewer-mode ; needs review
                     slack ; needs review
                     smex ; needs review
                     smooth-scrolling ; needs review
                     spaceline ; needs review
                     speed-type ; needs review
                     take-off ; needs review
                     tiny ; needs review
                     use-package ; needs review
                     visual-fill-column ; needs review
                     w3m ; needs review
                     web-beautify ; needs review
                     web-completion-data ; needs review
                     web-mode ; needs review
                     wgrep ; needs review
                     with-editor ; needs review
                     xcscope ; needs review
                     yaml-mode ; needs review
                     yapfify ; needs review
                     yasnippet ; needs review
		     ))

(prefer-coding-system 'utf-8) ;; fixes some packages containing non-iso-latin characets
(package-initialize)
(package-refresh-contents)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (progn
      (message "Installing: %s" package)
      (package-install package))))

(defun risky-local-variable-p (sym &optional _ignored) nil)

(setq settings-dir (expand-file-name "settings" user-emacs-directory))
(setq modes-dir (expand-file-name "modes" user-emacs-directory))
(setq experiments-dir (expand-file-name "experiments" user-emacs-directory))
(setq packages-dir (expand-file-name "packages" user-emacs-directory))
(setq privates-dir (expand-file-name "private" user-emacs-directory))
(setq experiments-dir (expand-file-name "experiments" user-emacs-directory))

(add-to-list 'load-path experiments-dir)
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path modes-dir)
(add-to-list 'load-path packages-dir)
(add-to-list 'load-path (expand-file-name "emacs-bazel-mode" packages-dir))

(defun to/my-require (symbol)
  "Requires a package and then prints that it was loaded"
  (require symbol)
  (message "Loaded config file: %s" (symbol-name symbol)))

(defun to/do-list-dir (thedir)
  "Iterates over all files in THEDIR and loads them"
  (if (file-accessible-directory-p thedir)
      (progn
        (dolist (file (directory-files thedir t "\.el$" nil))
          (load (file-name-sans-extension file)))
        (message "Loaded all files in: %s" thedir))
    (message "Did not load files in: %s, not accessible" thedir)))

(defmacro to/easy-hook
    (hook prog &optional local append  )
  `(add-hook ,hook (lambda () (interactive) ,prog) (when (boundp 'local) local)
    (when (boundp 'append) append)))

(to/my-require 'basic-setup)
(to/my-require 'input-setup)

;; store passphrases
(require 'plstore)
(setq plstore-cache-passphrase-for-symmetric-encryption t)


 (to/my-require 'fira-code-data)
; (to/my-require 'fira-code-mode)
(to/my-require 'communication-setup)
(to/my-require 'defuns)
(to/my-require 'project-setup)
(to/my-require 'modes-setup)
(to/my-require 'magit-setup)
(to/my-require 'company-setup)
(to/my-require 'flycheck-setup)
(to/my-require 'helm-setup)
(to/my-require 'darkroom-setup)
(to/my-require 'keybindings)
(to/my-require 'single-line-hooks)
(to/my-require 'kubernetes-porcelain)

;; Load modes
(to/do-list-dir modes-dir)
(to/do-list-dir privates-dir)
(to/do-list-dir experiments-dir)

;; Load appeareance and setup
(to/my-require 'appearance-setup)
(to/my-require 'diminish-setup)
(to/my-require 'scimax-org-babel-python)


(when (and (fboundp 'server-running-p)
           (not (server-running-p)))
  (server-start))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setenv "SSH_ASKPASS" "git-gui --askpass")

(to/easy-hook 'emacs-startup-hook (progn (to/my-require
                                                 'modeline-setup))
                                                 t)
