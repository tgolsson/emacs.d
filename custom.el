(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error nil)
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(minimap-highlight-line-color "#8f92d0")
 '(package-selected-packages
   (quote
    (dumb-jump elfeed elfeed-goodies elfeed-org take-off slack android-mode anzu auctex auctex-latexmk bind-key bison-mode cl-generic cmake-font-lock cmake-mode company company-arduino company-auctex company-c-headers company-cmake company-emacs-eclim company-irony company-irony-c-headers company-jedi company-lua company-math company-statistics company-web copy-as-format ctags darkroom dashboard demo-it diminish discover-my-major draft-mode emacs-eclim epresent fancy-battery fasd flx-ido flycheck flycheck-irony flycheck-pos-tip flymake-sass git-commit glsl-mode gnus-dired gradle-mode gscholar-bibtex guide-key guide-key-tip guru-mode helm helm-bibtex helm-core helm-descbinds helm-describe-modes helm-flx helm-flycheck helm-flyspell helm-fuzzier helm-ls-git helm-mu helm-projectile helm-smex helm-swoop helm-w32-launcher hide-lines hlinum html-script-src htmlize ibuffer-projectile irony java-snippets jdee jedi-core js2-highlight-vars js2-mode js2-refactor jtags latex-extra less-css-mode lua-mode magit magit-filenotify magit-popup markdown-mode markdown-mode+ markdown-preview-eww markdown-toc math-symbol-lists matlab-mode modern-cpp-font-lock mu4e mu4e-maildirs-extension multiple-cursors ob-browser ob-http org org-beautify-theme org-bullets org-mime org-present ox-impress-js ox-ioslide ox-reveal pos-tip powerline projectile python-django rainbow-mode rotate rtags sass-mode scss-mode smex smooth-scrolling spaceline speed-type tiny use-package use-package visual-fill-column w3m web-beautify web-completion-data web-mode yaml-mode yasnippet)))
 '(safe-local-variable-values
   (quote
    ((eval org-toggle-pretty-entities)
     (tab-width 4)
     (eval org-toggle-pretty-entites)
     (eval org-display-inline-images)
     (eval setq-local global-hl-line-mode nil)
     (eval face-remap-add-relative
           (quote shadow)
           (quote
            (:family "Fira Mono for Powerline")))
     (eval face-remap-add-relative
           (quote linum-highlight-face)
           (quote
            (:family "Fira Mono for Powerline" :height 100)))
     (eval face-remap-add-relative
           (quote linum)
           (quote
            (:family "Fira Mono for Powerline" :height 100)))
     (eval face-remap-add-relative
           (quote org-block)
           (quote
            (:family "Fira Mono for Powerline" :height 100)))
     (eval face-remap-add-relative
           (quote org-block-end-line)
           (quote
            (:family "Fira Mono for Powerline" :height 100)))
     (eval face-remap-add-relative
           (quote org-block-begin-line)
           (quote
            (:family "Fira Mono for Powerline" :height 100)))
     (eval face-remap-add-relative
           (quote default)
           (quote
            (:family "Segoe UI" :height 120)))
     (eval require
           (quote org-beautify-theme))
     (Eval auto-fill-mode -1)
     (python-shell-interpreter-args . "/home/tgo/Projects/SimpleMind/SimpleMind/manage.py shell")
     (python-shell-interpreter . "python")
     (eval setenv "TEXINPUTS" "/home/tgo/Dropbox/Dokument/latex/:"))))
 '(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:background "black" :foreground "green3"))))
 '(magit-diff-removed ((t (:background "black" :foreground "red3")))))
