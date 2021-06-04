(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-prefer-flymake nil
        lsp-prefer-capf t))


(use-package lsp-ui
  :commands lsp-ui-mode
  :bind-keymap ("C-c C-l" . lsp-command-map)
  :config
  (setq
   lsp-ui-sideline-enable t
   lsp-ui-flycheck-enable t
   lsp-ui-flycheck-list-position 'right
   lsp-ui-flycheck-live-reporting t)
  (puthash "GOPROXY" "https://proxy.golang.org,direct" lsp-go-env))


(provide 'lsp-setup)
