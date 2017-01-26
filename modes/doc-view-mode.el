
;; For things like LaTeX viewing
(add-hook 'doc-view-mode-hook (lambda () (interactive "")
                                (auto-revert-mode 0)
                                (linum-mode 0))
          )

