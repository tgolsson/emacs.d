(require 'jedi-core)
(require 'company-jedi)                          ; load company mode html backend
(require 'python-django)

(defun my-python-mode ()

  ;; make these variables local
  (setq python-indent-offset 4)
  (jedi:setup)
  
  (company-mode 1)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-jedi company-yasnippet))
  (setq python-django-qmgmt-runserver-default-bindaddr "0.0.0.0:8000") 
  )





(add-hook 'python-mode-hook 'my-python-mode)

