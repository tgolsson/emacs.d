;; GUI
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))


(global-hl-line-mode 1)
(show-paren-mode 1)

(add-to-list 'default-frame-alist '(foreground-color . "#ddd"))
(add-to-list 'default-frame-alist '(background-color . "#111"))
(set-face-background 'fringe "#111")

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(hl-line-mode)
(rainbow-mode)
(require 'minimap)
(if window-system
    (progn 
           (add-hook 'minimap-sb-mode-hook (lambda () (setq mode-line-format
                                                            nil)))
           (add-to-list 'minimap-major-modes 'latex-mode)
           (add-to-list 'minimap-major-modes 'cmake-mode)
           (minimap-mode)
           )
  )


(provide 'appearance)
