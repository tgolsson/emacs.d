
(use-package company
  :defer t
  :init
  (require 'company-irony)
  (require 'company-statistics)
  (require 'company-yasnippet)
  (require 'color)
  :config
  (setq company-tooltip-limit 20
        company-tooltip-align-annotations 't
        company-idle-delay .1
        company-begin-commasends '(lf-insert-command)
        company-minimum-prefix-length 1)
  (company-statistics-mode))


(provide 'setup-company)
