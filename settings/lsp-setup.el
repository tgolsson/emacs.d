(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil
        lsp-prefer-capf t))

(provide 'lsp-setup)
