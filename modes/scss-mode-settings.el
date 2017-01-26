(require 'company)
(require 'scss-mode)
(require 'company-css)
(defun my-scss-settings ()
  ""
  (company-mode 1)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-css)
  
  (add-hook 'after-save-hook (lambda () (interactive) (shell-command-to-string
  (format "scss %s %s.css" (buffer-name) (file-name-sans-extension
  (buffer-name))))) nil t)
  )

(add-hook 'scss-mode-hook 'my-scss-settings)
