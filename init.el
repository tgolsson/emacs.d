(setq package-enable-at-startup nil)
(package-initialize)
; (toggle-debug-on-error 1)
;; GENERAL SETTINGS
(require 'package) ;; You might already have this line
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))



(setq package-list '(
                     ac-html
                     ac-html-csswatcher
                     ac-ispell
                     ac-js2
                     ac-octave
                     ac-php-core
                     alert
                     android-mode
                     arduino-mode ;; configured
                     async
                     auctex 
                     auctex-latexmk
                     auto-complete 
                     auto-complete-auctex
                     bison-mode  
                     calfw  ;; configured
                     cmake-font-lock ;; configured 
                     cmake-mode ;; configured
                     company
                     company-arduino
                     company-auctex
                     company-c-headers
                     company-emacs-eclim
                     company-irony
                     company-irony-c-headers
                     company-jedi
                     company-lua
                     company-math
                     company-statistics
                     company-web
                     concurrent
                     ctable
                     ; ctags
                     darkroom ; configured
                     dash
                     dashboard
                     deferred
                     diminish
                     discover-my-major ; configured
                     draft-mode
                     eclim
                    ;; emacs-eclim
                     epc
                     epl
                     epresent
                     f
                     fancy-battery
                     fasd
                     fancy-battery
                     flycheck
                     git-commit
                     gntp
                     google-contacts
                     google-maps
                     gradle-mode
                     grizzl
                     guide-key
                     guide-key-tip      ;
                     guru-mode
                     helm
                     helm-smex
                     hide-lines
                     hlinum
                     html-script-src
                     htmlize
                     ibuffer-projectile
                     ido-at-point
                     ido-completing-read+
                     ido-ubiquitous
                     ido-vertical-mode
                     irony
                     java-snippets
                     jdee
                     jedi-core
                     js2-highlight-vars
                     js2-mode
                     js2-refactor
                     jtags
                     less-css-mode
                     let-alist
                     log4e
                     lua-mode
                     magit
                     magit-filenotify
                     magit-popup
                     makey
                     markdown-mode
                     markdown-mode+
                     markdown-toc
                     math-symbol-lists
                     matlab-mode
                     metaweblog
                     minimap
                     mu4e-maildirs-extension
                     multiple-cursors
                     neotree
                     oauth2
                     ob-browser
                     ob-http
                     org
                     org-caldav
                     org-gcal
                     org-present
                     org-projectile
                     org2blog
                     ox-html5slide
                     ox-impress-js
                     ox-ioslide
                     ox-reveal
		     page-break-lines
                     powerline
                     php-mode
                     pkg-info
                     popup
                     popwin
                     pos-tip
                     projectile
                     python-django
                     python-environment
                     rainbow-mode
                     request
                     request-deferred
                     rtags
                     spaceline
                     seq
                     simple-httpd
                     sass-mode
                     scss-mode
                     skewer-mode  ;; maybe remove
                     smex
                     smooth-scrolling
                     speed-type ; setup
                     tiny
                     w3m
                     web-beautify
                     web-completion-data
                     web-mode
                     with-editor
                     xcscope
                     xml-rpc
                     yasnippet ;; setup
                     use-package
		     ))


(package-initialize) 
(unless package-archive-contents
  (package-refresh-contents))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; store passphrases
(require 'plstore)
(setq plstore-cache-passphrase-for-symmetric-encryption t)


(setq settings-dir (expand-file-name "settings" user-emacs-directory))
(setq modes-dir (expand-file-name "modes" user-emacs-directory))
(setq experiments-dir (expand-file-name "experiments" user-emacs-directory))
(setq packages-dir (expand-file-name "packages" user-emacs-directory))
(setq privates-dir (expand-file-name "private" user-emacs-directory))

(add-to-list 'load-path settings-dir)
(add-to-list 'load-path modes-dir)
(add-to-list 'load-path packages-dir)
(require 'benchmark)

(defun to/my-require (symbol)
  "Requires a package and then prints that it was loaded"
  (let (tt (benchmark-elapse (require symbol)))
  (message "Loaded config file: %s in %s" (symbol-name symbol) 
    (if (numberp tt) tt 0))))

(defun to/do-list-dir (thedir)
  "Iterates over all files in THEDIR and loads them"
  (if (file-accessible-directory-p thedir)
      (progn
        (dolist (file (directory-files thedir t "\.el$" nil))
          (load (file-name-sans-extension file)))
        (message "Loaded all files in: %s" thedir))
    (message "Did not load files in: %s, not accessible", thedir)))

(defmacro to/easy-hook
    (hook prog &optional local append  )
  `(add-hook ,hook (lambda () (interactive) ,prog) (when (boundp 'local) local)
    (when (boundp 'append) append)))

(to/my-require 'basic-setup)
(to/my-require 'communication-setup)
(to/my-require 'input-setup)
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

;; Load modes
(to/do-list-dir modes-dir)
(to/do-list-dir privates-dir)
(to/do-list-dir experiments-dir)

;; Load appeareance and setup
(to/my-require 'appearance-setup)
(to/my-require 'diminish-setup)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setenv "SSH_ASKPASS" "git-gui --askpass")

(to/easy-hook 'emacs-startup-hook (progn (to/my-require
                                                 'modeline-setup))
                                                 t)

