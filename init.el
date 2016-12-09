(setq package-enable-at-startup nil) (package-initialize)

;(toggle-debug-on-error)
;; GENERAL SETTINGS
(require 'package) ;; You might already have this line
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
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
                     ctags
                     darkroom ; configured
                     dash
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
                     fasd
                     fancy-battery
                     flx
                     flx-ido
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
                     hide-lines
                     hlinum
                     html-script-src
                     htmlize
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

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq settings-dir (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)
(setq modes-dir (expand-file-name "modes" user-emacs-directory))
(add-to-list 'load-path modes-dir)

(setq packages-dir (expand-file-name "packages" user-emacs-directory))
(setq privates-dir (expand-file-name "private" user-emacs-directory))
(add-to-list 'load-path packages-dir)

(setq frame-background-mode 'dark)
(mapc 'frame-set-background-mode (frame-list))


(require 'basic-packages)
(message "basic-packages loaded")

(require 'settings)
(message "settings loaded")

(require 'defuns)
(message "defuns loaded")

(require 'modes)
(message "modes loaded")

(require 'setup-magit)
(message "magit setup")

(require 'setup-company)
(message "setup company")

(require 'setup-flycheck)
(message "flycheck setup")

(require 'setup-ido)
(message "ido setup")

(require 'darkroom-settings)
(message "darkroom setup")

(require 'linum-settings)
(message "linum setup")

(require 'projectile-settings)
(message "projectile setup")

(require 'keybindings)
(message "keybindings setup")

;; Load everything in modes file
(dolist (file (directory-files modes-dir t "\.el$" nil))
  (load (file-name-sans-extension file)))

;; Load everything in private folder
(dolist (file (directory-files privates-dir t "\.el$" nil))
  (load (file-name-sans-extension file)))

;; Load experiments files here! TODO: Make this nicer?
(load-file (expand-file-name "experiments/company-netlogo.el" user-emacs-directory))
(load-file (expand-file-name "experiments/fixme-mode.el" user-emacs-directory))
(message "experiments setup")

(setenv "SSH_ASKPASS" "git-gui--askpass")

(require 'appearance)
(message "appeareance loaded")

(require 'setup-diminish)
(message "diminish setup")
(add-hook 'emacs-startup-hook (lambda () (interactive)
                                (require 'modeline-settings)
                                (message "modeline setup")) t)
