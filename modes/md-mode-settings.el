
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (use-package company-emoji
    :commands company-emoji)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (abbrev-mode 1)
              (auto-fill-mode 0)
              (font-lock-mode 1)
              (add-to-list 'company-backends 'company-emoji)
              (add-to-list 'company-backends 'company-yasnippet))))
