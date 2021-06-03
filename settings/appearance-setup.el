
;;
;; GUI
;;

(blink-cursor-mode 0)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(setq
 cursor-type t
 font-lock-multiline t
 frame-background-mode 'dark
 inhibit-splash-screen t
 inhibit-startup-message t
 linum-delay t
 linum-eager t
 linum-format "%4d  " ; \u2502
 scroll-conservatively 100000
 scroll-margin 0
 scroll-preserve-screen-position 0
 frame-title-format '((:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b"))))

(setq-default font-lock-multiline t
              custom-safe-themes t
              custom-theme-directory (expand-file-name "themes" user-emacs-directory))


(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;
;; Theme and LNF
;;
(show-paren-mode 1)

(add-hook 'prog-mode-hook (lambda () "foo" (interactive) (when (window-system)
                                                      (cascadia-code-mode))))

(mapc 'frame-set-background-mode (frame-list))
;; (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(add-hook 'emacs-startup-hook (lambda () (interactive)
                                (require 'solarized-theme)
                                (load-theme 'solarized-light t)
                                (enable-theme 'solarized-light)))

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

(use-package emojify
  :hook (after-init . global-emojify-mode))

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
(add-hook 'after-make-frame-functions '--set-emoji-font)

(when window-system
  (set-frame-font "Cascadia Code 10")
  ;; (define-fringe-bitmap 'right-curly-arrow
  ;;   [#b00000000
  ;;    #b00000000
  ;;    #b00000000
  ;;    #b00000000
  ;;    #b00000000
  ;;    #b00000000
  ;;    #b00000000
  ;;    #b00000000])
  ;; (define-fringe-bitmap 'left-curly-arrow
  ;;   [#b00000000
  ;;    #b00000000
  ;;    #b00110000
  ;;    #b00110000
  ;;    #b00110000
  ;;    #b00110000
  ;;    #b00000000
  ;;    #b00000000])
  )



;;
;; Line numbers and line higlighting
;;
(global-hl-line-mode 1)
(hl-line-mode 1)

(use-package hlinum
  :defer 10
  :init
  (setq linum-highlight-in-all-buffersp t)
  :config
  (global-linum-mode 1)
  (linum-mode 1)
  (hlinum-activate))


(use-package rainbow-mode
  :defer 10)
;; (use-package beacon-mode
;;   :config
;;   (beacon-mode 1)
;;   (setq beacon-blink-when-point-moves-horizontally 1
;;         beacon-blink-when-point-moves-vertically 1
;;         beacon-blink-duration 0.1))

;; (use-package dashboard
;;             :init (dashboard-setup-startup-hook)
;;             :config
;;             (setq dashboard-items '((recents . 10)
;;                                     (projects . 10)
;;                                     (bookmarks . 5))))

(provide 'appearance-setup)
