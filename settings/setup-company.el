
(require 'company)
(require 'company-irony)
(require 'company-statistics)
(require 'company-yasnippet)
(require 'color)

(with-eval-after-load 'company 

  (company-statistics-mode)
                            
  (setq company-tooltip-limit 20)
  (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
  (setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
  (setq company-begin-commands '(self-insert-command)); start autocompletion only after typing
  (setq company-minimum-prefix-length 1))
                            

(let ( (bg (face-attribute 'default :background))
       (fg (face-attribute 'default :foreground)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg
                                                                            4)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face
       :background ,(color-lighten-name bg 10)))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face :foreground
                                          ,(color-darken-name fg 50) ))))))  
(provide 'setup-company)
