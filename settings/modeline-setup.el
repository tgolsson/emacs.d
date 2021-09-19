;; (require 'color)

;; (require 'spaceline)
;; (require 'spaceline-config)

;; (setq powerline-height 20
;;       spaceline-byte-compile t
;;       powerline-default-separator nil
;;       spaceline-minor-modes-separator " ")

;; (when (not (display-graphic-p))
;;   (setq powerline-default-separator 'utf-8))


;; ;; (spaceline-define-segment my-projectile
;; ;;   "Show the current projectile root."
;; ;;   (when (and (fboundp 'projectile-project-p)
;; ;;              (stringp (projectile-project-p))
;; ;;              (not (string= (projectile-project-name) (buffer-name))))
;; ;;     (concat "P: " (projectile-project-name)))
;; ;;   :when active
;; ;;   )

;; (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)

;; (spaceline-emacs-theme)
;; (spaceline-toggle-anzu-on)
;; (spaceline-toggle-helm-number-on)
;; (spaceline-toggle-line-on)
;; (spaceline-toggle-buffer-encoding-abbrev-off)
;; (spaceline-toggle-org-pomodoro-off)
;; (spaceline-toggle-column-off)
;; (spaceline-toggle-erc-track-off)
;; (spaceline-toggle-workspace-number-off)
;; (spaceline-toggle-window-number-off)
;; (spaceline-toggle-line-column-on)
;; (spaceline-toggle-mu4e-alert-segment-off)
;; (spaceline-toggle-buffer-size-off)
;; (spaceline-toggle-buffer-position-off)
;; (spaceline-toggle-hud-off)
;; (spaceline-toggle-selection-info-on)

;; (spaceline-helm-mode t)
;; (spaceline-info-mode t)
;; (spaceline-compile)

;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (let ((bg "#121213")
;;                   (fg "#c2b99f"))
;;               (set-face-attribute 'mode-line nil :box
;;                                   `(:line-width 2 :color ,(color-lighten-name bg
;;           10) :style nil ))
;;               (set-face-attribute 'mode-line-inactive nil :box
;;                                   `(:line-width 2 :color ,(color-darken-name bg 0) :style nil))
;;               (set-face-attribute 'mode-line-buffer-id nil :inherit nil)
;;               (set-face-attribute 'mode-line-buffer-id-inactive nil :inherit
;;                                   nil)

;;               (set-face-attribute 'spaceline-unmodified nil :background
;;           "#119900" :foreground "#b2a99f")
;;               (set-face-attribute 'spaceline-modified nil :background "#991100" :foreground "#b2a99f")
;;               (set-face-attribute 'spaceline-read-only nil :background "#110099"
;;                                   :foreground "#b2a99f")
;;               )

;; (let ((bg "#263238")
;;       (fg "#ffffff"))
;;   (set-face-attribute 'mode-line nil
;;                       :box `(:line-width 2 :color ,(color-lighten-name bg 20) :style nil )
;;                       :background (color-lighten-name bg 5) )
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :box `(:line-width 2 :color ,(color-darken-name bg 5) :style nil)
;;                       :background (color-darken-name bg 5))
;;   (set-face-attribute 'mode-line-buffer-id nil
;;                       :inherit nil
;;                       :background (color-lighten-name bg 5))
;;   (set-face-attribute 'mode-line-buffer-id-inactive nil
;;                       :inherit nil
;;                       :background (color-darken-name bg 5))
;;   (set-face-attribute 'spaceline-unmodified nil :background "#119900" :foreground "#b2a99f")
;;   (set-face-attribute 'spaceline-modified nil :background "#99150" :foreground "#b2a99f")
;;   (set-face-attribute 'spaceline-read-only nil :background "#15099" :foreground "#b2a99f")
;;   (set-face-attribute 'powerline-active2 nil :background (color-lighten-name bg 5))
;;   (set-face-attribute 'powerline-active1 nil :background (color-lighten-name bg 5))
;;   (set-face-attribute 'powerline-active0 nil :background (color-lighten-name bg 5))
;;   (set-face-attribute 'powerline-inactive2 nil :background (color-darken-name bg 5))
;;   (set-face-attribute 'powerline-inactive1 nil :background (color-darken-name bg 5))
;;   (set-face-attribute 'powerline-inactive0 nil :background (color-darken-name bg 5))
;;   )

(provide 'modeline-setup)
