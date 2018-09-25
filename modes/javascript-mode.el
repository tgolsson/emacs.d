(use-package js2-mode
  :ensure nil
  :mode "\\.js\\'"
  :init
  (setq js-indent-level 4))

(use-package js-auto-format-mode
  :ensure t
  :config
  (add-hook 'js-mode-hook #'js-auto-format-mode))
