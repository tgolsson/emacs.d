(require 'rainbow-mode)
(require 'fixme-mode)
;; GUI
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))


(global-hl-line-mode 1)
(show-paren-mode 1)

;; for basic setup
;; (add-to-list 'default-frame-alist '(foreground-color . "#ddd"))
;; (add-to-list 'default-frame-alist '(background-color . "#111"))
;; (set-face-background 'default "#111")
;; (set-face-foreground 'default "#ddd")
;; (set-face-background 'fringe "#111")
;; ;
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(hl-line-mode 1)

;; (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
;;   (lambda () (rainbow-mode 1)))
;; //(my-global-rainbow-mode 1)
(require 'minimap)
(if window-system
    (progn 
      (add-hook 'minimap-sb-mode-hook (lambda ()
                                        (setq mode-line-format nil)
                                        (visual-line-mode 1)
                                             ))
           (add-to-list 'minimap-major-modes 'latex-mode)
           (add-to-list 'minimap-major-modes 'cmake-mode)
           (add-to-list 'minimap-major-modes 'org-mode)
           (setq minimap-automatically-delete-window t)
           (minimap-mode)
           )
  )
;(set-face-attribute 'mode-line-inactive nil :box '(:line-width 2 :color "#333" ))
;(set-face-attribute 'mode-line nil :box '(:line-width 2 :color "#666" ))

(set-frame-font "Fira Mono for Powerline-9" t t)
; (set-frame-font "Source Code Pro for Powerline-9")

(setq custom-safe-themes t)
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
(setq minimap-minimum-width 20)
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-hook 'emacs-startup-hook (lambda () (interactive) (load-theme 'campfire t) ))

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
   #b00000000])
)
(provide 'appearance)


