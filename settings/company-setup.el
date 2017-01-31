
(use-package company
  :defer t
  :init
  (use-package company-irony)
  (use-package company-statistics)
  (require 'company-yasnippet)
  :config
  (setq company-tooltip-limit 20
        company-tooltip-align-annotations 't
        company-idle-delay .1
        company-begin-commasends '(lf-insert-command)
        company-minimum-prefix-length 1)
  (company-statistics-mode))


(provide 'company-setup)
