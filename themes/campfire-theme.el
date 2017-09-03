(require 'color)

(deftheme campfire
  "Created 2016-09-22.")

(let
    ((fg  "#b2a99f")
     (hl  "#327e02")
     (bg  "#121215")
     (hl2 "#ff0000")
     (hl3 "#b0322f")
     (hl4 "#6f72f0")
     (fg-term  "#d7a787")
     (hl-term "#5f8700")
     (bg-term  "#000000")
     (hl2-term "#ff0000")
     (hl3-term "#aff500")
     (hl4-term "#5f87ff"))
  ;; define actual faces
  (custom-theme-set-faces
   'campfire
   ;; base
   `(default                            ((((type graphic))  (:foreground ,fg :background ,bg ))
                                         (t ( :foreground ,fg-term :background ,bg-term ))))
   `(bold                               ((((type graphic)) (:inherit 'default :weight bold))
                                         (t (:inherit 'default :weight bold))))
   `(fringe                             ((((type graphic)) (:background ,bg :foreground ,hl2))
                                         (t (:background ,bg-term :foreground ,hl2-term))))
   `(cursor                             ((((type graphic)) (:background ,hl))
                                         (t (:background ,hl-term))))
   `(fixed-pitch                        ((((type graphic)) (:family "Monospace"))
                                         (t (:family "Monospace"))))
   `(variable-pitch                     ((((type graphic)) (:family "Sans Serif"))
                                         (t (:family "Sans Serif"))))
   `(escape-glyph                       ((((type graphic)) (:foreground ,hl))
                                         (t (:foreground ,hl-term))))
   `(minibuffer-prompt                  ((((type graphic)) (:foreground ,hl :background ,bg))
                                         (t (:foreground ,hl-term :background ,bg-term))))
   `(highlight                          ((((type graphic)) (:background ,(color-lighten-name bg 10)))
                                         (t (:background ,(color-lighten-name bg-term 10)))))
   `(region                             ((((type graphic)) (:background ,(color-darken-name bg 10)))
                                         (t (:background ,(color-darken-name bg-term 10)))))
   `(link                               ((((type graphic)) (:foreground ,(color-lighten-name
                                                                          hl4 20) :weight bold))
                                         (t (:foreground ,(color-lighten-name hl4-term 20) :weight bold))))
   ;; Font-lock
   `(font-lock-keyword-face             ((((type graphic)) (:foreground ,hl3 ))
                                         (t (:foreground ,hl3-term))))
   `(font-lock-constant-face            ((((type graphic)) (:foreground ,hl4 ))
                                         (t (:foreground ,hl4-term ))))
   `(font-lock-type-face                ((((type graphic)) (:foreground ,(color-darken-name fg
                                                                                            0) :weight bold))
                                         (t (:foreground ,(color-darken-name fg-term 0) :weight bold))))
   `(font-lock-doc-face                 ((((type graphic)) (:slant italic))
                                         (t (:slant italic))))
   `(font-lock-function-name-face       ((((type graphic)) (:foreground ,(color-lighten-name hl 5)))
                                         (t (:foreground ,(color-lighten-name hl-term 5)))))
   `(font-lock-variable-name-face       ((((type graphic)) (:foreground ,(color-lighten-name hl2 15)))
                                         (t (:foreground ,(color-lighten-name hl2-term 15)))))
   `(font-lock-string-face              ((((type graphic)) (:foreground ,(color-lighten-name fg
                                                                                             15) :slant italic))
                                         (t (:foreground ,(color-lighten-name fg-term 15) :slant italic))))
   `(font-lock-builtin-face             ((((type graphic)) (:foreground ,(color-darken-name fg 15)))
                                         (t (:foreground ,(color-darken-name
							   fg-term 15)))))
   `(font-lock-comment-face             ((((type graphic)) (:foreground ,(color-lighten-name bg 30)))
                                         (t (:foreground ,(color-lighten-name bg-term 30)))))
   `(font-lock-comment-delimiter-face   ((((type graphic)) (:foreground ,(color-lighten-name bg 30)))
                                         (t (:foreground ,(color-lighten-name bg-term 30)))))

   ;; flyspell faces
   `(flyspell-duplicate                 ((((type graphic))
                                          (:foreground ,fg :underline ( :color
									,hl-term :style line)))
                                         (t
                                          (:foreground ,fg-term :underline ( :color ,hl-term :style line)))))
   `(flyspell-incorrect                 ((((type graphic))
                                          (:foreground ,fg :underline ( :color ,hl2-term :style wave)))
                                         (t
                                          (:foreground ,fg :underline ( :color ,hl2-term :style wave)))))

   ;; linum
   `(linum-highlight-face               ((((type graphic)) (:inherit default :foreground ,hl2 ))
					 (t (:inherit default :foreground ,hl2-term ))))
   `(linum                              ((((type graphic)) (:inherit shadow :foreground ,(color-lighten-name bg 10) :background ,bg))
					 (t (:inherit shadow :foreground ,(color-lighten-name bg-term 10) :background ,bg-term))))

   ;; mu4e
   `(mu4e-header-highlight-face         (
                                         ((((type graphic)) (:inherit highlight))
                                          (t (:inherit highlight)))))
   `(mu4e-header-face                   (
                                         ((((type graphic)) (:inherit default))
                                          (t (:inherit default)))))
   `(mu4e-unread-face                   ((t (:inherit default :foreground ,(color-darken-name fg 15 ) :weight bold))))
   `(mu4e-read-face                     (
                                         ((((type graphic)) (:inherit default :foreground ,(color-lighten-name fg 15 )))
                                          (t (:inherit default :foreground ,(color-lighten-name fg 15 ))))))
   `(mu4e-replied-face                  (
                                         ((((type graphic)) (:inherit default :foreground ,hl))
                                          (t (:inherit default :foreground ,hl)))))

   ;; minimap faces
   `(minimap-active-region-background   (
                                         ((((type graphic)) (:height 15 :background ,(color-lighten-name bg 20)))
                                          (t (:height 15 :background ,(color-lighten-name bg 20))))))
   `(minimap-inactive-region-background (((((type graphic)) (:height 15 :background ,bg))
                                          (t (:height 15 :background ,bg)))))
   `(minimap-font-face                  ((t (:foreground ,(color-lighten-name hl 40) :height 15))))
   `(minimap-highlight-selected         ((t (:foreground ,(color-lighten-name bg 40) :background ,hl))))
   
   ;; magit
   `(magit-section-highlight            (
					 ((((type graphic)) (:background ,(color-lighten-name bg 10)))
					  (t (:background ,(color-lighten-name bg 10))))))
   `(magit-branch-local                 (
					 ((((type graphic)) (:foreground ,hl ))
					  (t (:foreground ,hl )))))
   `(magit-branch-remote                (
					 ((((type graphic)) (:foreground ,hl3 ))
					  (t (:foreground ,hl3 )))))
   `(magit-branch-current               (
					 ((((type graphic)) (:foreground ,hl4 ))
					  (t (:foreground ,hl4 )))))
   `(magit-section-heading              (
					 ((((type graphic)) (:weight bold :foreground ,(color-lighten-name hl2 10)))
					  (t (:weight bold :foreground ,(color-lighten-name hl2-term 10))))))
   
   ;; company
   `(company-tooltip                    ((((type graphic)) (:background ,(color-lighten-name bg 4) :inherit default ))
					 (t (:background ,(color-lighten-name bg-term 5) :inherit default ))))
   `(company-scrollbar-bg               ((((type graphic)) (:background ,(color-darken-name bg 10)))
					  (t (:background ,(color-darken-name bg-term 10)))))
   
   `(company-scrollbar-fg               ((((type graphic)) (:background ,(color-darken-name bg 5)))
					  (t (:background ,(color-darken-name hl-term 5)))))

   
   `(company-tooltip-selection          ((((type graphic)) (:inherit font-lock-function-name-face :background ,(color-darken-name bg 10)))
					  (t (:inherit font-lock-function-name-face :background ,(color-darken-name bg 10)))))
   
   `(company-tooltip-common             ((((type graphic)) (:inherit font-lock-constant-face :foreground ,hl2 ))
					  (t (:inherit font-lock-constant-face :foreground ,hl2-term ))))
 
   `(company-tooltip-common-selection             ((((type graphic)) (:inherit font-lock-constant-face :foreground ,hl2 ))
						    (t (:inherit font-lock-constant-face :foreground ,hl2-term :background ,(color-lighten-name bg 20) ))))
   
   `(company-preview                    ((((type graphic)) (:foreground ,(color-darken-name fg 10) :background ,(color-lighten-name bg 0)))
					 (t (:foreground ,(color-darken-name fg-term 10) :background ,(color-lighten-name bg 0)))))

   `(company-preview-search                    ((((type graphic)) (:foreground ,(color-darken-name fg 10) :background ,(color-lighten-name bg 0)))
						(t (:foreground ,(color-darken-name fg-term 10) :background ,(color-lighten-name bg 0)))))

  
   
   ;; mode-line
   `(fancy-battery-charging (
			     ((((type graphic)) (:foreground ,fg))
			      (t (:foreground ,fg-term)))))
   `(fancy-battery-critical (
			     ((((type graphic)) (:foreground ,hl2))
			      (t (:foreground ,hl2-term)))))
   `(fancy-battery-discharging (
				((((type graphic)) (:foreground ,hl3))
				 (t (:foreground ,hl3-term)))))

   ;; org-mode
   `(org-block-begin-line ((t (:foreground ,(color-lighten-name bg 20) :background ,bg))))
   `(org-block-end-line ((t (:foreground ,(color-lighten-name bg 20) :background ,bg))))
   `(org-block ((((type graphic)) (:foreground ,(color-darken-name fg 20) :background ,(color-lighten-name bg 10)))
		(t (:foreground ,(color-darken-name fg-term 20) :background ,(color-lighten-name bg-term 10)))))
   ;; END CUSTOM FACE

   ;; ediff
   `(ediff-odd-diff-B (
		       ((((type graphic)) (:background ,(color-lighten-name fg 10)))
			(t (:background ,(color-lighten-name fg-term 10))))))

   `(ediff-odd-even-B (
		       ((((type graphic)) (:background ,(color-darken-name fg 10)))
			(t (:background ,(color-darken-name fg-term 10))))))

   `(ediff-odd-diff-A (
		       ((((type graphic)) (:background ,(color-lighten-name fg 10)))
			(t (:background ,(color-lighten-name fg-term 10))))))

   `(ediff-odd-even-A (
		       ((((type graphic)) (:background ,(color-darken-name fg 10)))
			(t (:background ,(color-darken-name fg-term 10))))))
   ;; helm
   `(helm-selection (
		     ((((type graphic)) (:background ,hl :foreground ,bg))
		      (t (:background ,hl-term :foreground ,bg-term)))))
   `(helm-selection-line ((t (:background ,(color-lighten-name bg-term 10) :foreground ,fg-term))))

   ;; latex
   `(font-latex-sectioning-5-face (((((type graphic)) (:foreground ,hl))
				    (t (:foreground ,hl-term)))))))

  ;; (custom-theme-set-variables
  ;;  'campfire

  ;; ;; END CUSTOM VARIABLES
  ;; )
  (provide-theme 'campfire)
  ;; Local Variables:
  ;; eval: (auto-fill-mode -1)
  ;; End:
