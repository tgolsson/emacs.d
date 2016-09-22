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
  (setq web-mode-markup-indent-offset 2)

  
)

(flycheck-define-checker my-php
  "A PHP syntax checker using the PHP command line interpreter.

   See URL `http://php.net/manual/en/features.commandline.php'."
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
            "-d" "log_errors=0" source)
  :error-patterns
  ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
          (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php+-mode web-mode)
  )





(add-hook 'web-mode-hook 'my-web-mode)


