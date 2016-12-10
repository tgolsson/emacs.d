(require 'projectile)
(require 'neotree)
(require 'ibuffer-projectile)
(projectile-global-mode)
(setq neo-window-position 'right)
(setq projectile-switch-project-action 'neotree-projectile-action)

(provide 'projectile-settings)
