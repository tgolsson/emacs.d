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
		     arduino-mode
		     async
		     auctex
		     auctex-latexmk
		     auto-complete
		     auto-complete-auctex
		     cmake-mode
		     color-theme
		     company
		     company-arduino
		     company-auctex
		     company-c-headers
		     company-irony
		     company-irony-c-headers
			 company-jedi
		     company-math
		     company-statistics
		     company-web
		     ctags
		     dash
		     epl
		     f
		     flycheck
		     git-commit
		     gradle-mode
                     guru-mode
                     hide-lines
					 guide-key
		     html-script-src
		     irony
		     java-snippets
		     jdee
		     js2-highlight-vars
		     js2-mode
		     js2-refactor
		     jtags
		     less-css-mode
		     let-alist
		     magit
		     magit-popup
                     markdown-mode
                     markdown-mode+
                     markdown-toc
		     math-symbol-lists
		     matlab-mode
		     multiple-cursors
		     org
		     org-caldav
		     pkg-info
		     popup
			 popwin
		     s
		     simple-httpd
		     skewer-mode
		     web-completion-data
		     web-mode
		     with-editor
                     flx-ido
                     ido-at-point
                     ido-ubiquitous
                     ido-vertical-mode
                     smooth-scrolling
		     yasnippet
		     ))

(package-initialize) ;; You might already have this line

(unless package-archive-contents
  (package-refresh-contents))

                                        ; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq settings-dir (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)
(setq modes-dir (expand-file-name "modes" user-emacs-directory))
(add-to-list 'load-path modes-dir)

(setq packages-dir (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path packages-dir)

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

;; Load everything in settings file
(dolist (file (directory-files modes-dir t "\.el$" nil))
  (load (file-name-sans-extension file)))


