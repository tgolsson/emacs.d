;;
;; face for constant like mHello
;;
(defface c++-variable-proper-face
  '((t (:inherit font-lock-variable-name-face :inherit 'font-lock-variable-name-face)))
  "Face for single-letter prepended variable names, to highlight them slightly. Example: g_MyGlobal, l_MyLocal and so on."
  :group 'font-lock-faces )
(defvar c++-variable-proper-face 'c++-variable-proper-face)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords
             nil
             '(("[^a-zA-Z]\\([_]+[a-zA-Z0-9_]+\\)\\([\Z]\\|[^a-zA-Z\(]\\)" 1 c++-variable-proper-face prepend))  'append)))


;;
;; C-mode settings
;;
(use-package cc-mode
  :defer t
  :mode (("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode))  
  :bind (:map c-mode-base-map ("RET" . newline-and-indent))
  :init
  (setq-default c-default-style "linux"
                c-basic-offset 4)
  
                
  (c-set-offset 'inline-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'topmost-intro 0)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'member-init-intro 0)
  :config
  (add-hook 'c-mode-common-hook
            (lambda () (interactive "")
              (progn (company-mode 1)
                     (irony-mode 1)
                     (company-irony 1)
                     (electric-pair-mode 1)
                     (semantic-mode 1)
                     (make-local-variable 'company-backends)
                     (add-to-list 'company-backends '
                                  (company-irony company-irony-c-headers
                             company-yasnippet)) )) ) )


;;
;; Rtags
;; 
(use-package rtags
  :bind (( "M-." .  rtags-find-symbol-at-point)
         ( "M-," .  rtags-find-references-at-point)
         ( "C-." .  rtags-find-symbol)
         ( "C-," .  rtags-find-references)
         ( "C-<" .  rtags-find-virtuals-at-point)
         ( "M-i" .  rtags-imenu))
  :config 
  (setq rtags-completions-enabled t))


;;
;; irony-mode
;;
(use-package irony
  :defer t
  :init
  (require 'company-irony-c-headers)
  (require 'company-irony)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))





(provide 'cc-mode-settings)
