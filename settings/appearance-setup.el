
;;
;; GUI
;;

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'prog-mode-hook (lambda () "foo" (interactive) (when (window-system)
                                                      (cascadia-code-mode))))

(mapc 'frame-set-background-mode (frame-list))

(setq cursor-type t
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

(when window-system
  (set-frame-font "Cascadia Code 10")
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

(blink-cursor-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(use-package paren
  :commands show-paren-mode
  :init   (show-paren-mode +1))

(use-package solarized-theme
  :demand t
  :config
  (load-theme 'solarized-light t)
  (enable-theme 'solarized-light))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (defun to/set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (if (eq system-type 'darwin)
        ;; For NS/Cocoa
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
      ;; For Linux
      (set-fontset-font t 'symbol (font-spec :family "Symbola") frame
                        'prepend)))
  (to/set-emoji-font nil)
  (add-hook 'after-make-frame-functions 'to/set-emoji-font))


(use-package hl-line
  :demand t
  :init
  (global-hl-line-mode +1))

(use-package hlinum
  :defer 10
  :init
  (setq linum-highlight-in-all-buffersp t)
  :config
  (global-linum-mode 1)
  (linum-mode 1)
  (hlinum-activate))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))


(provide 'appearance-setup)
