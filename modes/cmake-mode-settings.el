(require 'company-cmake)
(require 'company-yasnippet)
(require 'cmake-font-lock)
(defun my-cmake-settings ()
  "Sets up environment for cmake"
  (make-local-variable 'company-backends)
  (setq company-backends '(company-cmake company-yasnippet))
  (company-mode 1)
  (cmake-font-lock-activate)
  )

(add-hook 'cmake-mode-hook 'my-cmake-settings)
