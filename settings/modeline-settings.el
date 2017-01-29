(require 'color)
(require 'spaceline)
(require 'spaceline-config)
(set-face-attribute 'mode-line-buffer-id nil :inherit nil)
(set-face-attribute 'mode-line-buffer-id-inactive nil :inherit nil)


;; SPACELINE
(let ((bg "#222")
      (fg "#c2b99f"))
  ;; active
  (set-face-attribute 'mode-line           nil                              :background (color-lighten-name bg 10) :foreground (color-lighten-name fg 10))
  (set-face-attribute 'powerline-active1   nil :inherit 'mode-line          :background (color-lighten-name bg 20) :foreground (color-lighten-name fg 20))
  (set-face-attribute 'powerline-active2   nil :inherit 'mode-line          :background (color-lighten-name bg 10) :foreground (color-lighten-name fg 10))
  ;; inactive
  (set-face-attribute 'mode-line-inactive  nil                              :background (color-lighten-name bg  0) :foreground (color-lighten-name  fg 0))
  (set-face-attribute 'powerline-inactive1 nil :inherit 'mode-line-inactive :background (color-lighten-name bg 10) :foreground (color-lighten-name fg 10))
  (set-face-attribute 'powerline-inactive2 nil :inherit 'mode-line-inactive :background (color-lighten-name bg  0) :foreground (color-lighten-name  fg  0))
  )

(setq powerline-height 20)

(defface pml-highlight
  '((t (:background "gray40")))
  "Basic face for highlighting.")

(when (not (display-graphic-p))
  (setq powerline-default-separator 'utf-8))

(spaceline-define-segment my-projectile
  "Show the current projectile root."
  (when (and (fboundp 'projectile-project-p)
             (stringp (projectile-project-p))
             (not (string= (projectile-project-name) (buffer-name))))
    (concat "P: " (projectile-project-name)))
  :when active
)

(spaceline-emacs-theme 'my-projectile)
(spaceline-toggle-anzu-on)
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



(add-hook 'window-setup-hook (lambda () 
                               (let ((bg "#222")
                                     (fg "#c2b99f"))
                                 (set-face-attribute 'mode-line nil :box
                                                     `(:line-width 2 :color ,(color-lighten-name bg 10)))
                                 (set-face-attribute 'mode-line-inactive nil :box
                                                     `(:line-width 2 :color ,(color-lighten-name bg 0)))
                                 (set-face-attribute 'mode-line-buffer-id nil :inherit nil)
                                 (set-face-attribute 'mode-line-buffer-id-inactive nil :inherit nil))))

(provide 'modeline-settings)



;; Local Variables:
;; Eval: (auto-fill-mode -1)
;; End:
