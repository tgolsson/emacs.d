(setq package-enable-at-startup nil)
(package-initialize)
;; GENERAL SETTINGS
(require 'package) ;; You might already have this line
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(setq use-package-always-ensure t)
(setq load-prefer-newer t)
(setq package-list '(android-mode ;; TODO 
                     anzu ;; TODO
                     arduino-mode ;; TODO 
                     auctex ;; FIXUP
                     auctex-latexmk ;; FIXUP
                     beacon ;; MANUAL DEPENDENCY
                     bind-key ;; DONE
                     bison-mode ;; NOT USED
                     cmake-font-lock ;; FIXUP
                     cmake-mode ;; FIXUP
                     company ;; TODO 
                     company-arduino ;; TODO
                     company-auctex ;; TODO 
                     company-emacs-eclim ;; TODO -- REMOVE?
                     company-irony ;; FIXUP
                     company-irony-c-headers ;; TODO
                     company-jedi ;; fixup 
                     company-lua ;; TODO 
                     company-math ;; what does this even do
                     company-statistics ;; fixup?
                     company-web ;; fixup
                     darkroom ;; fixup
                     dashboard ;; DONE
                     diminish ;; FIXUP
                     discover-my-major ;; TODO
                     draft-mode ;; what does this even do?
                     eclim ;; OK?
                     epresent ;; TEST
                     expand-region ;; TODO
                     fancy-battery ;; TODO 
                     flycheck ;; TODO
                     flycheck-irony ;; TODO
                     git-commit ;; TODO 
                     gradle-mode ;; TODO
                     gscholar-bibtex ;; TODO
                     guide-key ;; TODO 
                     guide-key-tip ;; TODO 
                     guru-mode ;; TODO 
                     helm ;; TODO
                     helm-bibtex ;; TODO
                     helm-core ;; TODO
                     helm-descbinds ;; TODO
                     helm-describe-modes ;; todo
                     helm-flx ;; TODO
                     helm-flycheck ;; TODO
                     helm-flyspell ;; TODO
                     helm-fuzzier ;; TODO
                     helm-ls-git ;; TODO
                     helm-mu ;; TODO
                     helm-projectile ;; TODO
                     helm-smex ;; TODO
                     helm-swoop ;; TODO
                     helm-w32-launcher ;;  TODO
                     hide-lines ;; TODO
                     hlinum ;; TODO
                     html-script-src ;; TODO
                     htmlize ;; TODO
                     ibuffer-projectile ;; TODO
                     irony ;; TODO
                     java-snippets ;; TODO
                     jdee ;; TODO
                     jedi-core ;; TODO
                     js2-highlight-vars ;; TODO
                     js2-mode ;; TODO
                     js2-refactor ;; TODO
                     jtags ;; TODO
                     less-css-mode ;; TODO
                     lua-mode ;; TODO
                     magit ;; TODO
                     magit-filenotify ;; TODO
                     magit-popup ;; TODO
                     markdown-mode ;; TODO
                     markdown-mode+ ;; TODO
                     markdown-preview-eww ;; TODO
                     markdown-toc ;; TODO
                     matlab-mode ;; TODO
                     mu4e-maildirs-extension ;; TODO
                     multiple-cursors ;; TODO
                     ob-browser ;; TODO
                     ob-http ;; TODO
                     org ;; TODO
                     org-beautify-theme ;; TODO
                     org-bullets ;; TODO
                     org-caldav ;; TODO
                     org-gcal ;; TODO
                     org-mime ;; TODO
                     org-present ;; TODO
                     org-projectile ;; TODO
                     ox-html5slide ;; TODO
                     ox-impress-js ;; TODO
                     ox-ioslide ;; TODO
                     ox-reveal ;; TODO
                     php-mode ;; TODO
                     pkg-info ;; TODO
                     popup ;; TODO
                     popwin ;; TODO
                     pos-tip ;; TODO
                     powerline ;; TODO
                     projectile ;; TODO
                     python-django ;; TODO
                     python-environment ;; TODO
                     rainbow-mode ;; TODO
                     request ;; TODO
                     request-deferred ;; TODO
                     rtags ;; TODO
                     sass-mode ;; TODO 
                     scss-mode ;; TODO 
                     seq ;; TODO
                     simple-httpd ;; TODO 
                     skewer-mode  ;; maybe remove
                     spaceline
                     speed-type ; setup
                     tiny ;; TODO
                     w3m ;; DONE
                     web-beautify ;; TODO
                     web-completion-data ;; 
                     web-mode
		     use-package
                     with-editor 
                     xcscope ;; TODO 
                     yasnippet ;; setup
		     ))

(prefer-coding-system 'utf-8) ;; fixes some packages containing non-iso-latin characets
(package-initialize) 
(package-refresh-contents)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



(setq settings-dir (expand-file-name "settings" user-emacs-directory))
(setq modes-dir (expand-file-name "modes" user-emacs-directory))
(setq experiments-dir (expand-file-name "experiments" user-emacs-directory))
(setq packages-dir (expand-file-name "packages" user-emacs-directory))
(setq privates-dir (expand-file-name "private" user-emacs-directory))

(add-to-list 'load-path settings-dir)
(add-to-list 'load-path modes-dir)
(add-to-list 'load-path packages-dir)

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
    (message "Did not load files in: %s, not accessible", thedir)))

(defmacro to/easy-hook
    (hook prog &optional local append  )
  `(add-hook ,hook (lambda () (interactive) ,prog) (when (boundp 'local) local)
    (when (boundp 'append) append)))

(to/my-require 'basic-setup)
(to/my-require 'input-setup)

;; store passphrases
(require 'plstore)
(setq plstore-cache-passphrase-for-symmetric-encryption t)


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


