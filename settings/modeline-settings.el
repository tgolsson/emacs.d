(require 'smart-mode-line)
(require 'fancy-battery)
(deftheme smart-mode-line-mine "Light theme for smart-mode-line.")


(setq sml/theme 'dark)
(setq sml/mode-width 'right)
(sml/setup)
(fancy-battery-mode 1)
(set-face-background 'mode-line "#222")

(setq sml/pre-modes-separator (propertize (concat (char-to-string 57520) "  ")
                                                          'face 
                                                          '(:foreground "#222"
                                                                        :background "#444")))


(setq sml/pre-minor-modes-separator (propertize (concat  "  " (char-to-string 57522))
                                                          'face 
                                                          '(:foreground "#222"
                                                                        :background "#444")))

(set-face-background 'sml/modes "#444")
(set-face-background 'sml/line-number "#222")


(provide 'modeline-settings)
