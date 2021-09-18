(use-package company-web)

(use-package web-mode  
  :mode ("\\.phtml\\'" "\\.html\\'" "\\.svelte\\'")
  :config
  (use-package company-web-html :commands web-mode)
  (use-package company-web-jade :commands web-mode)
  (use-package company-web-slim :commands web-mode)
  (use-package prettier-js :hook web-mode)
  (use-package add-node-modules-path :hook web-mode)
  :init
  (defun my-web-mode ()
    (make-local-variable 'yas-extra-modes)
    (add-to-list 'yas-extra-modes 'html-mode)
    (add-to-list 'yas-extra-modes 'php-mode)
    ;; make these variables local
    (make-local-variable 'web-mode-code-indent-offset)
    (make-local-variable 'web-mode-markup-indent-offset)
    (make-local-variable 'web-mode-css-indent-offset)

    (make-local-variable 'company-backends)
    (add-to-list 'company-backends '(company-web-html company-yasnippet))
    (define-key web-mode-map (kbd "C-<Space>") 'company-web-html)
    (company-mode 1)

    ;; set indentation, can set different indentation level for different code type
    (setq web-mode-code-indent-offset 4
          web-mode-css-indent-offset 4
          web-mode-markup-indent-offset 4
          web-mode-script-padding 4
          prettier-js-args '("--tab-width" "4"))
    
    (prettier-js-mode 1)
    (add-node-modules-path 1))
  
  (add-hook 'web-mode-hook 'my-web-mode))


