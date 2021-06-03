
(use-package company
  :commands company-mode
  :init
  (use-package company-statistics
    :hook (company-mode . company-statistics-mode))
  (use-package company-quickhelp
    :hook (company-mode . company-quickhelp-mode))
  :config
  (define-key company-active-map (kbd "TAB")   'tab-indent-or-complete)
  (define-key company-active-map (kbd "<tab>") 'tab-indent-or-complete)

  (setq company-tooltip-limit 20
        company-tooltip-align-annotations 't
        company-idle-delay .1
        company-begin-commasends '(lf-insert-command)
        company-minimum-prefix-length 1))

(provide 'company-setup)
