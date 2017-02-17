

(use-package projectile
  :bind ("C-x M-k" . to/kill-other-buffers)
  :config 
  (projectile-global-mode 1)
  (setq projectile-switch-project-action 'magit-status))

(use-package ibuffer-projectile)

(provide 'project-setup)
