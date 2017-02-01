
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
(blink-cursor-mode -1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;;
;; Minimap
;;
(use-package minimap
  :if window-system
  :config  
  (add-hook 'minimap-sb-mode-hook (lambda ()
                                    (setq mode-line-format nil)
                                    (visual-line-mode 1)))
  (add-to-list 'minimap-major-modes '(latex-mode cmake-mode org-mode))       
  (setq minimap-minimum-width 20
        minimap-automatically-delete-window t)
  (minimap-mode 1))


;;
;; Theme and LNF
;;
(show-paren-mode 1)
(set-frame-font "Source Code Pro for Powerline-9" t t)
################################################################################
(setq frame-background-mode 'dark)
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
(require 'fixme-mode)


(use-package dashboard
            :init (dashboard-setup-startup-hook)
            :config
            (setq dashboard-items '((recents . 10)
                                    (projects . 10)
                                    (bookmarks . 5))))

(provide 'appearance-setup)


