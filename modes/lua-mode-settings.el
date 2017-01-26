(require 'company-lua)                          ; load company lua
(require 'lua-mode)


(defun my-lua-mode ()
  ;; make these variables local

  (company-mode 1)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-lua company-yasnippet)) 
  )





(add-hook 'lua-mode-hook 'my-lua-mode)


