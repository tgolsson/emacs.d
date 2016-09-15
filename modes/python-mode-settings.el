(require 'company-jedi)                          ; load company mode html backend


(defun my-python-mode ()

  ;; make these variables local

  (company-mode)
  (add-to-list 'company-backends '(company-jedi company-yasnippet))

  )





(add-hook 'python-mode-hook 'my-python-mode)


