                     ;; go-eldoc
                     ;; go-gopath
                     ;; go-mode
                     ;; go-projectile
                     ;; golint

(use-package go-mode
  :mode "\\.go\\'"
  :init
  ;; (use-package company-go :hook go-mode)
  (use-package flycheck-golangci-lint :commands flycheck-golangci-lint-setup)
  ;; (use-package go-eldoc :hook go-mode)
  ;; (use-package go-gopath :hook go-mode)
  ;; (use-package go-impl :hook go-mode)
  (use-package go-projectile :commands go-projectile-tools-add-path :hook (go-mode . go-projectile-set-gopath))
  :config
  (defun to/my-go-mode ()
    ;; (define-key go-mode-map (kbd "C-c C-e") #'go-gopath-set-gopath)
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    ;; (go-eldoc-setup)
    (company-mode 1)
    (go-projectile-tools-add-path)
    (make-local-variable 'company-backends)
    (set (make-local-variable 'company-backends) '(company-capf))
    (lsp)
    (lsp-mode 1)
    (flycheck-golangci-lint-setup)
    (flycheck-mode 1)
    (flycheck-pos-tip-mode 1)
    (flycheck-add-next-checker 'lsp 'golangci-lint)
    (add-hook 'before-save-hook 'gofmt-before-save nil t))
  (add-hook 'go-mode-hook 'to/my-go-mode))
