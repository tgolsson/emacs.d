(require 'markdown-mode)

(defun my-markdown-settings ()
  (abbrev-mode 1)
  (auto-fill-mode 0)
  (font-lock-mode 1)
  (markdown-mode 1)
  (add-to-list 'company-backends 'company-yasnippet)
)



(add-hook 'markdown-mode-hook 'my-markdown-settings)
