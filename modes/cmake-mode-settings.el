(require 'company-cmake)
(require 'company-yasnippet)
(defun my-cmake-settings ()
  "Sets up environment for cmake"
  (make-local-variable 'company-backends)
  (setq company-backends '(company-cmake company-yasnippet))
  )

(add-hook 'cmake-mode-hook 'my-cmake-settings)
