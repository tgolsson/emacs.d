;; Make commit-lines 72 lines max
(add-hook 'git-commit-mode-hook (lambda () (interactive "") (set-fill-column 72)))

(provide 'single-line-hooks)
