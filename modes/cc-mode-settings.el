;; ;;
;; ;; face for constant like mHello
;; ;;
;; (defface c++-variable-proper-face
;;   '((t (:inherit font-lock-variable-name-face :inherit 'font-lock-variable-name-face)))
;;   "Face for single-letter prepended variable names, to highlight them slightly. Example: g_MyGlobal, l_MyLocal and so on."
;;   :group 'font-lock-faces )
;; (defvar c++-variable-proper-face 'c++-variable-proper-face)
;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (font-lock-add-keywords
;;              nil
;;              '(("[^a-zA-Z_]\\([msgMSG]?_[a-zA-Z0-9]+[a-zA-Z0-9_]+\\)\\([\Z]\\|[^a-zA-Z\(]\\)" 1 c++-variable-proper-face keep))  'append)))

;; (defface c++-macro-face
;;   '((t (:inherit font-lock-keyword-face :inherit 'font-lock-keyword-face)))
;;   "Face for dunder macro, to highlight them slightly. Example: __FUNCTION__ and so on."
;;   :group 'font-lock-faces )
;; (defvar c++-macro-face 'c++-macro-face)
;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (font-lock-add-keywords
;;              nil
;;              '(("[^a-zA-Z_]\\(__[a-zA-Z0-9_]+__\\)\\([\Z]\\|[^a-zA-Z\(]\\)" 1 c++-macro-face keep))  'append)))


;;
;; C-mode settings
;;
(use-package cc-mode
  :defer t
  :mode (("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.c\\'" . c-mode))
  :bind (:map c-mode-base-map ("RET" . newline-and-indent))
  :init
  (setq-default c-default-style "linux"
                c-basic-offset 4)
  :config
  (c-set-offset 'inline-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'topmost-intro 0)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'member-init-intro 0)
  (add-hook 'c-mode-common-hook
            (lambda () (interactive "")
              (progn (company-mode 1)
                     (irony-mode 1)
                     (company-irony 1)
                     (electric-pair-mode 1)
                     (flycheck-mode)
                     (make-local-variable 'company-backends)
                     (add-hook 'before-save-hook 'clang-format-buffer t t)
                     (add-to-list 'company-backends '
                                  (company-irony company-irony-c-headers
                             company-yasnippet)) )) ) )


;;
;; Rtags
;;
;; (use-package rtags
;;   :bind (( "M-." .  rtags-find-symbol-at-point)
;;          ( "M-," .  rtags-find-references-at-point)
;;          ( "C-." .  rtags-find-symbol)
;;          ( "C-," .  rtags-find-references)
;;          ( "C-<" .  rtags-find-virtuals-at-point)
;;          ( "M-i" .  rtags-imenu))
;;   :config
;;   (setq rtags-completions-enabled t))


;;
;; irony-mode
;;
(use-package irony
  :hook cc-mode
  :defer t
  :init
  (use-package company-irony-c-headers
    :commands company-irony-c-headers)
  (use-package company-irony
    :commands company-irony)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; Redefinition of function from gud.el
(defun gud-find-expr (&rest args)
  (let ((expr (if (and transient-mark-mode mark-active)
		  (buffer-substring (region-beginning) (region-end))
		(apply gud-find-expr-function args))))
    (save-match-data
      (if (string-match "\n" expr)
	  (error "Expression must not include a newline"))
      (with-current-buffer gud-comint-buffer
	(save-excursion
	  (goto-char (process-mark (get-buffer-process gud-comint-buffer)))
	  (forward-line 0)
	  (when (looking-at comint-prompt-regexp)
	    (set-marker gud-delete-prompt-marker (point))
	    (set-marker-insertion-type gud-delete-prompt-marker t))
	  (unless (eq (buffer-local-value 'gud-minor-mode gud-comint-buffer)
		      'jdb)
	    (message (concat expr " = "))))))
    expr))

(provide 'cc-mode-settings)
