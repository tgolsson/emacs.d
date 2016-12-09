(require 'cc-mode)
(require 'rtags)
(require 'company-rtags)
(require 'company-irony)
(require 'irony)
(require 'company-irony-c-headers)
;; C-mode SETTINGS
(setq-default c-default-style "linux"
              c-basic-offset 4)
(c-set-offset 'inline-open 0)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)


;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


(defun setup-c-modes ()
  (company-mode 1)
  (irony-mode 1)
  (company-irony 1)
  (electric-pair-mode 1)
  (semantic-mode 1)
  (make-local-variable 'company-backends)
  (setq rtags-completions-enabled t)
  (add-to-list 'company-backends '(company-irony company-irony-c-headers
                                                 company-yasnippet)))
(add-hook 'c-mode-common-hook 'setup-c-modes)


(defface c++-variable-proper-face
  '((t (:inherit font-lock-variable-name-face :inherit 'font-lock-variable-name-face)))
  "Face for single-letter prepended variable names, to highlight them slightly. Example: g_MyGlobal, l_MyLocal and so on."
  :group 'font-lock-faces )
(defvar c++-variable-proper-face 'c++-variable-proper-face)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("[^a-zA-Z]\\([_]+[a-zA-Z0-9_]+\\)\\([\Z]\\|[^a-zA-Z\(]\\)" 1 c++-variable-proper-face prepend))  'append
				    )))

(define-key c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,") (function rtags-find-references-at-point))
(define-key c-mode-base-map (kbd "C-.") (function rtags-find-symbol))
(define-key c-mode-base-map (kbd "C-,") (function rtags-find-references))
(define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
(define-key c-mode-base-map (kbd "M-i") (function rtags-imenu))


(provide 'cc-mode-settings)
