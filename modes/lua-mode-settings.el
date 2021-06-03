(use-package lua-mode
  :mode "\\.lua\\'"
  :init
  (use-package company-lua
    :commands company-lua)                          ; load company lua
  :config
  (add-hook 'lua-mode-hook
            (lambda ()
              (company-mode 1)
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends '(company-lua company-yasnippet)))))
