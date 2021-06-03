
(use-package go-mode
  :mode "\\.go\\'"
  :init
  (use-package company-go :mode "\\.go\\'")
  (use-package flycheck-golangci-lint :mode "\\.go\\'")
  (use-package go-eldoc :mode "\\.go\\'")
  (use-package go-gopath :mode "\\.go\\'")
  (use-package go-impl :mode "\\.go\\'")
  (use-package go-projectile :mode "\\.go\\'")
  (use-package godoctor :mode "\\.go\\'")
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              ;; (define-key go-mode-map (kbd "C-c C-e") #'go-gopath-set-gopath)
              ;; (add-hook 'before-save-hook 'gofmt-before-save)
              ;; (go-eldoc-setup)
              (company-mode 1)
              ;; (go-projectile-tools-add-path)

              (make-local-variable 'company-backends)
              (set (make-local-variable 'company-backends) '(company-capf))
              (setq lsp-prefer-flymake nil)
              (lsp)
              (lsp-mode 1)
              (add-hook 'before-save-hook 'gofmt-before-save))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my-go-mode.el ends here
