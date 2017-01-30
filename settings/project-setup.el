
(use-package neotree
  :bind ("<f8>" . neotree-toggle)
  :config 
  (setq neo-window-position 'right))

(use-package projectile
  :bind ("C-x M-k" . to/kill-other-buffers)
  :config 
  (projectile-global-mode 1)
  (setq projectile-switch-project-action 'neotree-projectile-action))
(use-package ibuffer-projectile)

(provide 'project-setup)
