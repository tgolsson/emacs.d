(require 'cc-mode)

;; C-mode SETTINGS
(setq-default c-default-style "linux"
              c-basic-offset 4
              )
(c-set-offset 'inline-open 0)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)


(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


(defun setup-c-modes ()
  (company-mode 1)
  (irony-mode 1)
  (company-irony 1)
  (electric-pair-mode 1)
  (semantic-mode 1)
(make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-irony company-irony-c-headers
  company-etags company-yasnippet))
  
  ) 
(add-hook 'c-mode-common-hook 'setup-c-modes)


(defface c++-variable-proper-face
  '((t (:inherit font-lock-variable-name-face :foreground "#ffffaa")))
  "Face for single-letter prepended variable names, to highlight them slightly. Example: g_MyGlobal, l_MyLocal and so on."
  :group 'font-lock-faces )
(defvar c++-variable-proper-face 'c++-variable-proper-face)
(add-hook 'c-mode-common-hook
	  (lambda ()                    
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))


(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("[^a-zA-Z]\\([a-zA-Z][_]+[a-zA-Z0-9_]+\\)\\([\Z]\\|[^a-zA-Z\(]\\)" 1 c++-variable-proper-face prepend))  'append
				    )))


