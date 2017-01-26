(require 'company-web-html)                          ; load company mode html backend
(require 'company-web-jade)                          ; load company mode jade backend
(require 'company-web-slim)                          ; load company mode slim backend

(require 'web-mode)
;; you may key bind, for example for web-mode:


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
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 4)

  
)

(add-hook 'web-mode-hook 'my-web-mode)


