(require 'color)

(deftheme campfire
  "Created 2016-09-22.")

(let ((fg  "#b2a99f")
      (hl  "#327e02")
      (bg  "#121215")
      (hl2 "#ff0000")
      (hl3 "#b0322f")
      (hl4 "#6f72f0"))
  ;; define actual faces
(custom-theme-set-faces
   'campfire
   ;; base
   `(default                            ((t ( :foreground ,fg :background ,bg ))))
   `(bold ((t (:inherit 'default :weight bold))))
   `(fringe                             ((t (:background ,bg :foreground ,hl2))))
   `(cursor                             ((t (:background ,hl)))) 
   `(fixed-pitch                        ((t (:family "Monospace"))))
   `(variable-pitch                     ((t (:family "Sans Serif"))))
   `(escape-glyph                       ((t (:foreground ,hl))))
   `(minibuffer-prompt                  ((t (:foreground ,hl :background ,bg))))
   `(highlight                          ((t (:background ,(color-lighten-name bg 10)))))
   `(region                             ((t (:background ,(color-darken-name bg 10)))))
   `(link                               ((t (:foreground ,(color-lighten-name hl4 20) :weight bold))))  
   ;; Font-lock
   `(font-lock-keyword-face             ((t (:foreground ,hl3 ))))
   `(font-lock-constant-face            ((t (:foreground ,hl4 ))))
   `(font-lock-type-face                ((t (:foreground ,(color-darken-name fg 0) :weight bold))))
   `(font-lock-doc-face                 ((t (:slant italic))))
   `(font-lock-function-name-face       ((t (:foreground ,(color-lighten-name hl 5)))))
   `(font-lock-variable-name-face       ((t (:foreground ,(color-lighten-name hl2 15)))))
   `(font-lock-string-face              ((t (:foreground ,(color-lighten-name fg 15) :slant italic))))
   `(font-lock-builtin-face             ((t (:foreground ,(color-darken-name fg 15)))))
   `(font-lock-comment-face             ((t (:foreground ,(color-lighten-name bg 30)))))
   `(font-lock-comment-delimiter-face   ((t (:foreground ,(color-lighten-name bg 30)))))

   ;; flyspell faces
   `(flyspell-duplicate                 ((t (:foreground ,fg :underline ( :color ,hl :style line)))))
   `(flyspell-incorrect                 ((t (:foreground ,fg :underline ( :color ,hl2 :style wave)))))
   
   ;; linum
   `(linum-highlight-face               ((t (:inherit default :foreground ,hl2 ))))
   `(linum                              ((t (:inherit shadow :foreground ,(color-lighten-name bg 10) :background ,bg))))
   
   ;; mu4e
   `(mu4e-header-highlight-face         ((t (:inherit highlight))))
   `(mu4e-header-face                   ((t (:inherit default))))
   `(mu4e-unread-face                   ((t (:inherit default :foreground ,(color-darken-name fg 15 ) :weight bold))))
   `(mu4e-read-face                     ((t (:inherit default :foreground ,(color-lighten-name fg 15 )))))
   `(mu4e-replied-face                  ((t (:inherit default :foreground ,hl))))
   
   ;; minimap faces
   `(minimap-active-region-background   ((t (:height 15 :background ,(color-lighten-name bg 20)))))
   `(minimap-inactive-region-background ((t (:height 15 :background ,bg))))
   `(minimap-font-face                  ((t (:foreground ,(color-lighten-name hl 40) :height 15))))
   `(minimap-highlight-selected         ((t (:foreground ,(color-lighten-name bg 40) :background ,hl))))

   ;; magit
    `(magit-section-highlight            ((t (:background ,(color-lighten-name bg 10)))))
    `(magit-branch-local                 ((t (:foreground ,hl ))))
    `(magit-branch-remote                ((t (:foreground ,hl3 ))))
    `(magit-branch-current               ((t (:foreground ,hl4 ))))
   `(magit-section-heading              ((t (:weight bold :foreground ,(color-lighten-name hl2 10)))))
   
   ;; company
   `(company-tooltip                    ((t (:background ,(color-lighten-name bg 4) :inherit default ))))
   `(company-scrollbar-bg               ((t (:background ,(color-darken-name bg 10)))))
   `(company-scrollbar-fg               ((t (:background ,(color-darken-name bg 5)))))
   `(company-tooltip-selection          ((t (:inherit font-lock-function-name-face :background ,(color-darken-name bg 10)))))
   `(company-tooltip-common             ((t (:inherit font-lock-constant-face :foreground ,hl2 ))))
   `(company-preview                    ((t (:foreground ,(color-darken-name fg 10) :background ,(color-lighten-name bg 0)))))
   
   
   ;; mode-line
   `(fancy-battery)
   `(fancy-battery-charging ((t (:foreground ,fg))))
   `(fancy-battery-critical ((t (:foreground ,hl2))))
   `(fancy-battery-discharging ((t (:foreground ,hl3))))

   ;; org-mode
   `(org-block-begin-line ((t (:foreground ,(color-lighten-name bg 20) :background ,bg))))
   `(org-block-end-line ((t (:foreground ,(color-lighten-name bg 20) :background ,bg))))
   `(org-block ((t (:foreground ,(color-darken-name fg 20) :background ,(color-lighten-name bg 10)))))
   ;; END CUSTOM FACE

   ;; ediff
   `(ediff-odd-diff-B ((t (:background "#444449"))))
   `(ediff-even-diff-B ((t (:background "#5a5555")))) 
   `(ediff-odd-diff-A ((t (:background "#444449"))))
   `(ediff-even-diff-A ((t (:background "#5a5555"))))

   ;; helm
   `(helm-selection ((t (:background ,hl :foreground ,bg))))
   `(helm-selection-line ((t (:background ,(color-lighten-name bg 10) :foreground ,fg))))

   ;; latex
   `(font-latex-sectioning-5-face ((t (:foreground ,hl))))
  ))
  ;; (custom-theme-set-variables
  ;;  'campfire
   
  ;; ;; END CUSTOM VARIABLES
  ;; )
(provide-theme 'campfire)
;; Local Variables:
;; eval: (auto-fill-mode -1)
;; End:
