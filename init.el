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
                     arduino-mode
                     async
                     auctex
                     auctex-latexmk
                     auto-complete
                     auto-complete-auctex
                     bison-mode
                     calfw
                     cmake-font-lock
                     cmake-mode
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
                     discover-my-major ; configured
                     draft-mode
                     eclim
                     emacs-eclim
                     epc
                     epl
                     epresent
                     f
                     fasd
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
                     php-mode
                     pkg-info
                     popup
                     popwin
                     pos-tip
                     projectile
                     python-environment
                     rainbow-mode
                     request
                     request-deferred
                     rtags
                     s
                     seq
                     simple-httpd
                     skewer-mode
                     smex
                     smooth-scrolling
                     speed-type
                     tiny
                     w3m
                     web-beautify
                     web-completion-data
                     web-mode
                     with-editor
                     xcscope
                     xml-rpc
                     yasnippet

		     ))

(package-initialize) ;; You might already have this line

(unless package-archive-contents
  (package-refresh-contents))


                                        ; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load experiments files here!
(load-file (expand-file-name "experiments/company-netlogo.el" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq settings-dir (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)
(setq modes-dir (expand-file-name "modes" user-emacs-directory))
(add-to-list 'load-path modes-dir)

(setq packages-dir (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path packages-dir)

(setq frame-background-mode 'dark)
(mapc 'frame-set-background-mode (frame-list))
(require 'appearance)

(require 'basic-packages)
(require 'settings)
(require 'defuns)

(require 'keybindings)
(require 'settings)

(require 'modes)
(require 'setup-magit)
(require 'setup-company)
(require 'setup-flycheck)
(require 'setup-ido)
(require 'darkroom-settings)
(require 'modeline-settings)
(require 'linum-settings)
;; Load everything in settings file
(dolist (file (directory-files modes-dir t "\.el$" nil))
  (load (file-name-sans-extension file)))


(setenv "SSH_ASKPASS" "git-gui--askpass")
