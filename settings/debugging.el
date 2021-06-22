
(use-package dap-mode
  :commands dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :init
  (use-package treemacs)
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup)

  (require 'dap-go)
  (dap-go-setup)
  (require 'dap-hydra)

  
  (require 'dap-gdb-lldb)


  (dap-gdb-lldb-setup)
  
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra)))
  
(provide 'debugging)
