(use-package projectile
  :ensure t
  :defer 10
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-switch-project-action 'magit-status
        projectile-globally-ignored-directories (append
                                                 '("*__pycache__*" "*.egg-info")
                                                 projectile-globally-ignored-directories)
        projectile-globally-ignored-file-suffixes (append
                                                   '(".pyc")
                                                   projectile-globally-ignored-file-suffixes)
        projectile-indexing-method 'alien
        projectile-enable-caching 't
        projectile-git-command "fd . -0"
        projectile-git-submodule-command "git submodule --quiet foreach 'echo $path'")
  (projectile-mode +1)
  (use-package ibuffer-projectile)
  (projectile-global-mode 1)
  (helm-projectile-on))

;; (use-package helm-projectile
;;   :ensure t
;;   :demand t
;;   :config
;;   (progn
;;     (setq projectile-completion-system 'helm)
;;     (helm-projectile-on)
;;     )
;;   )



(provide 'project-setup)
