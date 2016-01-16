(ido-mode t)
(setq ido-enable-flex-matching t)

;; GUI
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))


(global-hl-line-mode 1)
(show-paren-mode 1)

(add-to-list 'default-frame-alist '(foreground-color . "#aaa099"))
(add-to-list 'default-frame-alist '(background-color . "#000000"))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(provide 'appearance)
