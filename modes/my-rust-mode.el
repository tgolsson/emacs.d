

;; (use-package cargo
;;   :
;;   :ensure t
;;   :defer t)

;; (use-package helm-lsp
;;   :commands )

;; (use-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

      ;;                cargo
(defun my-rust-mode-hook()
  (set (make-local-variable 'compile-command) "cargo run")

  (if (eq system-type 'windows-nt)
      (progn
        (add-to-list 'exec-path "C:/Users/Tom/.cargo/bin"))
    (progn
      (add-to-list 'exec-path "~/.cargo/bin")))

  (setq lsp-rust-server 'rust-analyzer
        lsp-headerline-breadcrumb-enable nil
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-ui-doc-use-childframe nil
        lsp-prefer-capf nil)
  (lsp 1)
  (lsp-mode 1)
  (lsp-lens-mode 1)
  (lsp-rust-analyzer-inlay-hints-mode 1)
  ;; (company-mode 1)
  ;; (cargo-minor-mode 1)
  (flycheck-mode 1)
  (hs-minor-mode 1)
  (flycheck-pos-tip-mode 0)
  (flycheck-inline-mode 0)
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(use-package rust-mode
  :hook (rust-mode . lsp)
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook 'my-rust-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rust-mode.el ends here
