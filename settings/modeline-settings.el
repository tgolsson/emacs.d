(require 'spaceline)
(require 'spaceline-config)
(require 'fancy-battery)
(fancy-battery-mode)

;; DIMINISH
(require 'diminish)
(eval-after-load "minimap-mode" (diminish 'minimap-mode))
(eval-after-load "guide-key-mode" (diminish 'guide-key-mode))
(eval-after-load "abbrev-mode" (diminish 'abbrev-mode))
(eval-after-load "yasnippet-mode" (diminish 'yas-minor-mode))
(eval-after-load "auto-revert-mode" (diminish 'auto-revert-mode))
(eval-after-load "auto-fill" (diminish 'auto-fill-function))
(eval-after-load "flyspell-mode" (diminish 'flyspell-mode))
(eval-after-load "flycheck-mode" (diminish 'flycheck-mode))
(require 'eldoc)
(eval-after-load "eldoc-mode" (diminish 'eldoc-mode))
(eval-after-load "subword-mode" (diminish 'subword-mode))
(eval-after-load "projectile-mode" (diminish 'projectile-mode))
(eval-after-load "rainbow" (diminish 'rainbow-mode))

;; SPACELINE
(set-face-background 'mode-line "gray40")
(set-face-foreground 'powerline-active1 "#999")
(setq powerline-height 25)

(defface pml-highlight
  '((t (:background "gray40")))
  "Basic face for highlighting.")
(when (not (display-graphic-p))
  (setq powerline-default-separator 'utf-8)
  )
(spaceline-define-segment my-projectile
  "Show the current projectile root."
  (when (and (fboundp 'projectile-project-p)
             (stringp (projectile-project-p))
             (not (string= (projectile-project-name) (buffer-name))))
    (concat "P: " (projectile-project-name)))
  :when active
)

(spaceline-emacs-theme 'my-projectile)

;; 
(spaceline-toggle-anzu-off)
(spaceline-toggle-battery-on)
(spaceline-toggle-line-on)
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-org-pomodoro-off)
(spaceline-toggle-column-off)
(spaceline-toggle-erc-track-off)
(spaceline-toggle-workspace-number-off)
(spaceline-toggle-window-number-off)
(spaceline-toggle-line-column-on)
(spaceline-toggle-mu4e-alert-segment-on)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-position-off)
(setq spaceline-minor-modes-separator " ")

(set-face-attribute 'mode-line-inactive nil :box '(:line-width 2 :color "#333" ))
(set-face-attribute 'mode-line nil :box '(:line-width 2 :color "#666" ))
(provide 'modeline-settings)
