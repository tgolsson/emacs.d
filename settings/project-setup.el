

(use-package projectile
  :bind ("C-x M-k" . to/kill-other-buffers)
  :config 
  (projectile-global-mode 1)
  (setq projectile-switch-project-action 'magit-status
        projectile-globally-ignored-directories (append
                                                 '("*__pycache__*" "*.egg-info")
                                                 projectile-globally-ignored-directories)
        projectile-globally-ignored-file-suffixes (append
                                                   '(".pyc")
                                                   projectile-globally-ignored-file-suffixes)))

(use-package ibuffer-projectile)

(provide 'project-setup)
