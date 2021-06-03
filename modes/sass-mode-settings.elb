(require 'company)
(require 'sass-mode)
(require 'company-css)
(defun my-sass-settings ()
  ""
  (company-mode 1)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-css)
  
  (add-hook 'after-save-hook (lambda () (interactive) (shell-command-to-string
                                                       (format "sassc %s %s.css"
                                                       (buffer-name)
                                                       (file-name-sans-extension
                                                       (buffer-name))))) nil t)
  )
(defconst sass-line-keywords
  '(("@\\(\\w+\\)"    0 font-lock-keyword-face sass-highlight-directive)
("/[/*].*"  0 font-lock-comment-face)
("[=+]\\w+" 0 font-lock-variable-name-face sass-highlight-script-after-match)
("!\\w+"    0 font-lock-variable-name-face sass-highlight-script-after-match)
(":\\w+"    0 font-lock-variable-name-face)
("\\w+\s*:" 0 font-lock-variable-name-face)
("\\(\\w+\\)\s*="  1 font-lock-variable-name-face sass-highlight-script-after-match)
("\\(:\\w+\\)\s*=" 1 font-lock-variable-name-face sass-highlight-script-after-match)
(".*"      sass-highlight-selector)))

(defconst sass-selector-font-lock-keywords
  '( ;; Attribute selectors (e.g. p[foo=bar])
("\\[\\([^]=]+\\)" (1 font-lock-variable-name-face)
 ("[~|$^*]?=\\([^]=]+\\)" nil nil (1 font-lock-string-face)))
("&"       0 font-lock-constant-face)
("\\.\\w+" 0 font-lock-type-face)
("#\\w+"   0 font-lock-keyword-face)
;; Pseudo-selectors, optionally with arguments (e.g. :first, :nth-child(12))
("\\(::?\\w+\\)" (1 font-lock-variable-name-face)
 ("(\\([^)]+\\))" nil nil (1 font-lock-string-face)))))
(defconst sass-non-block-openers
  '("^.*,$" ;; Continued selectors
"^ *@\\(extend\\|debug\\|warn\\|include\\|import\\)" ;; Single-line mixins
"^ *[$!]"     ;; Variables
".*[^\s-]+: [^\s-]" ;; a setting of some sort
))
(add-hook 'sass-mode-hook 'my-sass-settings)
