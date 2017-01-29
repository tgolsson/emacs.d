(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode))
  :config
  (use-package company-cmake)
  (use-package cmake-font-lock)
  :init
  (add-hook 'cmake-mode-hook '(lambda () (interactive "")
                                (make-local-variable 'company-backends)
                                (setq company-backends '(company-cmake company-yasnippet))
                                (company-mode 1)
                                (cmake-font-lock-activate))))
