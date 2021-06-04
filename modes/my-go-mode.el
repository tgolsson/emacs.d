(use-package go-mode
  :mode "\\.go\\'"
  :init
  (use-package company-go :hook go-mode)
  (use-package flycheck-golangci-lint :hook go-mode)
  (use-package go-eldoc :hook go-mode)
  (use-package go-gopath :hook go-mode)
  (use-package go-impl :hook go-mode)
  (use-package godoctor :hook go-mode)
  (use-package go-projectile :commands go-projectile-tools-add-path :hook go-mode)
  :init
  ;; (eval-after-load "lsp-mode" '(puthash "GOPROXY" "https://proxy.golang.org,direct" lsp-go-env))
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
    (add-hook 'before-save-hook 'gofmt-before-save nil t))
  (add-hook 'go-mode-hook 'to/my-go-mode))
