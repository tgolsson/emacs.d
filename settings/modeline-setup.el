(require 'color)
(require 'spaceline)
(require 'spaceline-config)
(set-face-attribute 'mode-line-buffer-id nil :inherit nil)
(set-face-attribute 'mode-line-buffer-id-inactive nil :inherit nil)


;; SPACELINE
(let ((bg "#121213")
      (fg "#c2b99f"))
  ;; active
  (set-face-attribute 'mode-line           nil
                      :background (color-lighten-name bg 10) :foreground
                      (color-lighten-name fg 10))
  (set-face-attribute 'powerline-active1   nil :inherit 'mode-line :background (color-lighten-name bg 20) :foreground (color-lighten-name fg 20))
  (set-face-attribute 'powerline-active2   nil :inherit 'mode-line :background (color-lighten-name bg 10) :foreground (color-lighten-name fg 10))
  ;; inactive
  (set-face-attribute 'mode-line-inactive  nil :background (color-darken-name bg  1) :foreground (color-lighten-name  fg 0))
  (set-face-attribute 'powerline-inactive1 nil :inherit 'mode-line-inactive :background (color-lighten-name bg 10) :foreground (color-lighten-name fg 10))
  (set-face-attribute 'powerline-inactive2 nil :inherit 'mode-line-inactive :background (color-darken-name bg 1) :foreground (color-lighten-name  fg  0)))

(setq powerline-height 20)

(defface pml-highlight
  '((t (:background "gray40")))
  "Basic face for highlighting.")

(setq powerline-default-separator nil)
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
(setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
(spaceline-emacs-theme 'my-projectile)
(spaceline-toggle-anzu-on)
(spaceline-toggle-helm-number-on)
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
(spaceline-toggle-hud-off)
(spaceline-toggle-selection-info-on)

(setq spaceline-minor-modes-separator " ")
(spaceline-helm-mode t)


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

(let ((bg "#121213")                                                                      
      (fg "#c2b99f"))                                                               
  (set-face-attribute 'mode-line nil :box                                                 
                      `(:line-width 2 :color ,(color-lighten-name bg 10) :style nil ))                                                                          
  (set-face-attribute 'mode-line-inactive nil :box                                        
                      `(:line-width 2 :color ,(color-darken-name bg 0) :style nil))       
                      (set-face-attribute 'mode-line-buffer-id nil :inherit nil)                              
                      (set-face-attribute 'mode-line-buffer-id-inactive nil :inherit                          
                                          nil)                                                                
                      
                      (set-face-attribute 'spaceline-unmodified nil :background                               
                                          "#119900" :foreground "#b2a99f")                                                             
                      (set-face-attribute 'spaceline-modified nil :background "#991100" :foreground "#b2a99f") 
                      (set-face-attribute 'spaceline-read-only nil :background "#110099"                      
                                          :foreground "#b2a99f"))
(spaceline-compile)
(provide 'modeline-setup)
