
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ba01a0672820fd7e8699d4b4275102ec392fd18b4d98bf9e60ba283f5612d499" "fc6eff2e29001b4447abd2fe4bf624118d44d6c51d42937683f20484ba4838bc" "468a601e7a3592bda46e41ee68577dadb35e9a729b5eba63f6aaa969374b196c" "4eb2a252a39c81fc0f4830ee54883a86da59cce1ae1ffa5d3b96752d51dd259f" "62d5a5cb991444b0ef1086221bc10edb67f0f036202e5c0ab695159c755d5af0" "f5df38c6e76ce4bccb80fed2a40dbd288847a4a582b3c5e64babcb9ccafa71d5" "11efd7cd69b47db84da516e43f99bd991dfe9843a18ca67fb1b0a3d9b0b0ac6e" "733aa0b681cf1359a845e158cf9a62e734d208d39798b8a17472e37cc69fa006" "7788410669c8acd04c697b01e59af3a32bd9fd0c1e950c7d6057eced577c14da" "033ef0d36ac63d76e26a724f9fd4cc07c72b946aac49e1b35909719790c24d29" "668962b51c58139735154a532377e004de493acad6a665d187894f28ef8ffc85" "4c8e59dff36f814ff89d62d4cd8f5f0bb6220dd9123c9e340b414f5223f2548b" "f0a6831a90ca3f937bb1f474ca5007f617f191caaea2028c9398baaf5954c7ec" "5c896c251731c7c1841b42f91134e8a50c61542a5763a501e77bd91261ef0bae" "b2f0a9a435b6a854147b336317e63145eb776c36af389e8a0913163a301c1fb3" "fc00124c4b50f89715169fa3f6bc8679407258692a7939e5b07ca6bcd664711d" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(minimap-highlight-line-color "#8f92d0")
 '(package-selected-packages
   (quote
    (flycheck-irony scss-mode flymake-sass sass-mode python-django web-mode web-beautify w3m visual-fill-column tiny speed-type spaceline smooth-scrolling smex smart-mode-line-powerline-theme rtags rotate rainbow-mode ox-reveal ox-ioslide ox-impress-js org2blog org-projectile org-present org-gcal org-caldav ob-http ob-browser neotree minimap matlab-mode markdown-toc markdown-preview-eww markdown-mode+ magit-filenotify less-css-mode jtags js2-refactor js2-highlight-vars jdee java-snippets ido-vertical-mode ido-ubiquitous ido-at-point htmlize html-script-src hlinum hide-lines guru-mode guide-key-tip gradle-mode google-contacts flycheck flx-ido fasd fancy-battery epresent emacs-eclim draft-mode discover-my-major diminish darkroom ctags company-web company-statistics company-math company-lua company-jedi company-irony-c-headers company-emacs-eclim company-auctex company-arduino cmake-font-lock cl-generic calfw bison-mode auto-complete-auctex auctex-latexmk android-mode ac-php-core ac-octave ac-js2 ac-ispell ac-html)))
 '(safe-local-variable-values
   (quote
    ((python-shell-interpreter-args . "/home/tgo/Projects/SimpleMind/SimpleMind/manage.py shell")
     (python-shell-interpreter . "python")
     (eval setenv "TEXINPUTS" "/home/tgo/Dropbox/Dokument/latex/:"))))
 '(send-mail-function (quote sendmail-send-it)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(company-scrollbar-bg ((t (:background "#191919"))))
;;  '(company-scrollbar-fg ((t (:background "#0c0c0c"))))
;;  '(company-tooltip ((t (:inherit default :background "#0a0a0a"))))
;;  '(company-tooltip-common ((t (:inherit font-lock-constant-face :foreground "#7f7f7f"))))
;;  '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;  '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face :foreground "color-33" :height 1.1))))
;;  '(font-latex-sectioning-5-face ((t (:foreground "color-20" :weight bold))))
;;  '(font-lock-negation-char-face ((t (:foreground "red"))))
;;  '(magit-diff-added ((t (:background "black" :foreground "green3"))))
;;  '(magit-diff-removed ((t (:background "black" :foreground "red3")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#191919"))))
 '(company-scrollbar-fg ((t (:background "#0c0c0c"))))
 '(company-tooltip ((t (:inherit default :background "#0a0a0a"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face :foreground "#7f7f7f"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face :background "#191919"))))
 '(magit-diff-added ((t (:background "black" :foreground "green3"))))
 '(magit-diff-removed ((t (:background "black" :foreground "red3"))))
 '(mode-line-buffer-id ((t (:inherit powerline-active2)))))
