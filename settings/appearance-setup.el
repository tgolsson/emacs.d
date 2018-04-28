(require 'color)
;;
;; GUI
;;
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(setq inhibit-splash-screen t
      inhibit-startup-message t)
(blink-cursor-mode 0)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 0)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;
;; Theme and LNF
;;
(show-paren-mode 1)

(set-frame-font
 (if (display-graphic-p)
     (case (window-system)
       ('w32 "Source Code Pro-9.5")
       ('x "Source Code Pro-9.5"))
  (case (window-system)
    ('w32 "Sauce Code Powerline-9.5")
    ('x "Source Code Pro for Powerline-9.5"))) t t)

;; (cond
;;  ((member "Source Code Pro" (font-family-list))
;;   (set-frame-font "Source Code Pro-9" t t))
;;  ((member "Sauce Code Powerline" (font-family-list))
;;   (set-frame-font "Sauce Code Powerline-9" t t))
;;  ((t) "Consolas")
;;  )

(setq frame-background-mode 'dark
      font-lock-multiline t)

(setq-default font-lock-multiline t)
(mapc 'frame-set-background-mode (frame-list))
(setq custom-safe-themes t
      custom-theme-directory (expand-file-name "themes" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-hook 'emacs-startup-hook (lambda () (interactive) (load-theme 'campfire t)))

(when window-system
  (define-fringe-bitmap 'right-curly-arrow
    [#b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00000000
     #b00000000
     #b00110000
     #b00110000
     #b00110000
     #b00110000
     #b00000000
     #b00000000]))


;;
;; Line numbers and line higlighting
;;
(global-hl-line-mode 1)
(hl-line-mode 1)
(setq linum-format "%4d  " ; \u2502
      linum-delay t
      linum-eager t)

(use-package hlinum
  :config
  (global-linum-mode 1)
  (linum-mode 1)
  (hlinum-activate)
  (setq linum-highlight-in-all-buffersp t))


(use-package rainbow-mode)
; (require 'fixme-mode)


(use-package dashboard
            :init (dashboard-setup-startup-hook)
            :config
            (setq dashboard-items '((recents . 10)
                                    (projects . 10)
                                    (bookmarks . 5))))

(provide 'appearance-setup)
