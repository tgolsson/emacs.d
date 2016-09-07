(require 'netlogo-mode)

(defun my-netlogo-settings ()
  (abbrev-mode 1)
  (auto-fill-mode 0)
  (font-lock-mode 1)
  (company-mode 1)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-yasnippet))
)



(add-hook 'netlogo-mode-hook 'my-netlogo-settings)
