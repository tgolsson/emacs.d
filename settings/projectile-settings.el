(require 'projectile)
(require 'neotree)
(projectile-global-mode)
(setq neo-window-position 'right)
(setq projectile-switch-project-action 'neotree-projectile-action)

(provide 'projectile-settings)
