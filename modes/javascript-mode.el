(defun my-js-mode-hook ()
  "Setups up my local javascript settings"
  (interactive "P")
  (setq js-indent-level 2)
  )
(add-hook 'js-mode-hook 'my-js-mode-hook)
