
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
                            

;; (let ( (bg (face-attribute 'default :background))
;;        (fg (face-attribute 'default :foreground)))
;;   (custom-set-faces
;; ))

(provide 'setup-company)
